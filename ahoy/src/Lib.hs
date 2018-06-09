{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson
import qualified Data.Aeson                 as A
import           Data.Aeson.TH
import qualified Data.ByteString            as BSS
import qualified Data.ByteString.Char8      as BCS
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.HashMap.Strict        (lookup)
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Scientific            as Scientific
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Database.Redis             as Redis
import           GHC.Exts
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server             (err400, err404, err500)

hostPrefix = "http://localhost:1234/obj/"

type ObjId = BSS.ByteString

type API = "outbox" :> ReqBody '[JSON] A.Value :> Post '[JSON] A.Value
      :<|> "inbox"  :> Capture "objid" String :> Get  '[JSON] A.Value

startApp :: IO ()
startApp = do
  dbh <- Redis.checkedConnect Redis.defaultConnectInfo
  Redis.runRedis dbh $ do
     Redis.set "hello" "hello"
     Redis.set "world" "world"
     hello <- Redis.get "hello"
     world <- Redis.get "world"
     fail <- Redis.get "this-will-fail"
     liftIO $ print (hello, world, fail)
  putStrLn "Starting on port 1234"
  run 1234 $ serve api (server dbh)

api :: Proxy API
api = Proxy


server dbh = outboxPost
        :<|> inboxGet
    where
      outboxPost o@(Object obj) = do
        result <- liftIO $ putObjIntoDb dbh (Object obj)
        case result of
          (Right err) -> throwError $ err400 {
            errBody = BC.concat ["Object error ",  A.encode o, ": ", (BC.pack (show err))]
          }
          (Left objWithId) -> do
            liftIO $ BC.putStrLn $ "Posted: " `BS.append` (A.encode objWithId)
            return $ Object $ fromList [ ("success", A.String "You did it!")
                                   , ("posted",  objWithId)
                                   ]
      outboxPost _ = do
        throwError $ err400 { errBody = "Must be JSON object" }

      inboxGet textobjid = do
        let objid = BCS.pack textobjid
        result <- liftIO $ getObjFromDb dbh objid
        case result of
          (Left obj) -> return obj
          (Right err) -> throwError $ err404 {
            errBody = BS.concat ["Can't get object ", BC.pack ((show objid) ++ ": " ++ (show err))]
          }

lookupObj :: T.Text -> Value -> Maybe Value
lookupObj param (A.Object obj) = HashMap.lookup param obj
lookupObj _ _                  = Nothing

data ObjIdError = ObjIdErrorNoId | ObjIdErrorBadId deriving (Show, Read)

getObjId :: Value -> Either ObjId ObjIdError
getObjId o@(A.Object obj) =
  case lookupObj "id" o of
    (Just idval) -> case idval of
      (A.String idstr) -> Left (TE.encodeUtf8 idstr)
      _                -> Right ObjIdErrorBadId
    _ -> Right ObjIdErrorNoId


data PutObjError = PutObjErrorId ObjIdError
                 | PutObjErrorCantGenerateId
                 | PutObjErrorDbError
                 | PutObjErrorBadType
                 deriving (Show)

putObjIntoDb :: Redis.Connection -> A.Value -> IO (Either Value PutObjError)
putObjIntoDb dbh o@(Object _) = withId o (getObjId o)
    where
      withId :: Value -> Either ObjId ObjIdError -> IO (Either Value PutObjError)
      withId o (Left objid) = actuallyPutIntoDb o objid
      withId o (Right _) = do
        newObjIdM <- getNewObjId dbh
        withNewId o newObjIdM

      withNewId :: Value -> (Maybe ObjId) -> IO (Either Value PutObjError)
      withNewId o Nothing = return $ Right PutObjErrorCantGenerateId
      withNewId o@(Object obj) (Just newObjId) = do
        let objHashWithId = HashMap.insert "id" (toJSON $ TE.decodeUtf8 newObjId) obj
        actuallyPutIntoDb (Object objHashWithId) newObjId

      actuallyPutIntoDb :: Value -> ObjId -> IO (Either Value PutObjError)
      actuallyPutIntoDb o@(Object _) id = do
        result <- Redis.runRedis dbh $
          Redis.set (BS.toStrict (BC.pack ("obj:"++(show id))))
                    (BS.toStrict (A.encode o))
        return $ doneRedis o result
      actuallyPutIntoDb _ _ = return (Right PutObjErrorBadType)

      doneRedis :: A.Value -> Either Redis.Reply Redis.Status -> Either Value PutObjError
      doneRedis o (Right Redis.Ok) = Left o
      doneRedis o _                = Right PutObjErrorDbError


str2redis :: String -> BSS.ByteString
str2redis str = BS.toStrict $ BC.pack $ str


data GetObjError = GetObjErrorNotFound
                 | GetObjErrorBadDatatype
                 | GetObjErrorCantDecode
                 | GetObjErrorDbError
                 deriving (Show)

getObjFromDb :: Redis.Connection -> ObjId -> IO (Either Value GetObjError)
getObjFromDb dbh objid = do
  redisResult <- Redis.runRedis dbh $ Redis.get (str2redis ("obj:" ++ (show objid)))
  return $ case redisResult of
    (Right (Just jsonstr)) ->
      case A.decode $ BS.fromStrict jsonstr of
        (Just jsonval) -> Left jsonval
        Nothing        -> Right GetObjErrorCantDecode
    (Right Nothing) -> Right GetObjErrorNotFound
    (Left _) -> Right GetObjErrorDbError

getNewObjId :: Redis.Connection -> IO (Maybe ObjId)
getNewObjId dbh = do
  result <- liftIO $ Redis.runRedis dbh $ Redis.incr "counter_objid"
  return $ case result of
    (Right val) -> Just (BSS.append hostPrefix $ BS.toStrict $ BC.pack $ show $ val)
    (Left _)    -> Nothing
