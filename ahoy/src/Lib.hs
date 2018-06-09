{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( startApp
    ) where

import GHC.Exts
import Data.Aeson
import qualified Data.Aeson as A
import qualified Database.Redis as Redis
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server (err400, err404)
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Strict (lookup)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.ByteString as BSS
import qualified Data.Scientific as Scientific

hostPrefix = "http://localhost:1234/obj/"

type ObjId = Integer

type API = "outbox" :> ReqBody '[JSON] A.Value :> Post '[JSON] A.Value
      :<|> "inbox"  :> Capture "objid" ObjId :> Get  '[JSON] A.Value

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
      outboxPost (Object obj) = do
        objWithId <- putObjIntoDb dbh (Object obj)
        liftIO $ do
          BC.putStrLn $ "Posted: " `BS.append` (A.encode objWithId)
        return $ Object $ fromList [ ("success", A.String "You did it!")
                                   , ("posted",  objWithId)
                                   ]
      outboxPost _ = do
        throwError $ err400 { errBody = "Must be JSON object" }

      inboxGet objid = do
        result <- getObjFromDb dbh objid
        case result of
          (Left obj) -> return obj
          (Right err) -> throwError $ err404 {
            errBody = BS.concat ["Can't get object ", BC.pack ((show objid) ++ (show err))]
          }

lookupObj :: T.Text -> Value -> Maybe Value
lookupObj param (A.Object obj) = HashMap.lookup param obj
lookupObj _ _ = Nothing

data ObjIdError = ObjIdErrorNoId | ObjIdErrorBadId deriving (Show, Read)

getObjId :: Value -> Either ObjId ObjIdError
getObjId o@(A.Object obj) =
  case lookupObj "id" o of
    (Just (A.Number idsci)) ->
      case Scientific.floatingOrInteger idsci of
        (Right idint) -> Left idint
        (Left _) -> Right ObjIdErrorBadId
    _ -> Right ObjIdErrorNoId

putObjIntoDb :: Redis.Connection -> A.Value -> Handler Value
putObjIntoDb dbh o@(Object obj) = do
  case getObjId o of
    (Left objId) -> actuallyPutIntoDb o
    (Right _) -> do
        newObjId <- getNewObjId dbh
        let objHashWithId = HashMap.insert "id" (toJSON newObjId) obj
        actuallyPutIntoDb (Object objHashWithId)
    where
      actuallyPutIntoDb o@(Object _) =
        case getObjId o of
          (Left objId) -> do
            result <- liftIO $ Redis.runRedis dbh $
              Redis.set (BS.toStrict (BC.pack ("obj:"++(show objId))))
                        (BS.toStrict (A.encode o))
            return o
          (Right err) -> throwError $ err400 {
            errBody = BC.concat ["Object error ",  A.encode o, ": ", (BC.pack (show err))]
            }


str2redis :: String -> BSS.ByteString
str2redis str = BS.toStrict $ BC.pack $ str


data GetObjError = GetObjErrorNotFound
                 | GetObjErrorBadDatatype
                 | GetObjErrorCantDecode
                 | GetObjErrorDbError
                 deriving (Show)

getObjFromDb :: Redis.Connection -> ObjId -> Handler (Either Value GetObjError)
getObjFromDb dbh objid = do
  redisResult <- liftIO $ redisResultDo
  return $ case redisResult of
    (Left jsonstr) -> case A.decode $ BS.fromStrict jsonstr of
      (Just jsonval) -> Left jsonval
      Nothing -> Right GetObjErrorCantDecode
    (Right err) -> Right err
  where
    redisResultDo :: IO (Either BSS.ByteString GetObjError)
    redisResultDo = do
      result <- Redis.runRedis dbh $ Redis.get (str2redis ("obj:" ++ (show objid)))
      return $ case result of
        (Right (Just jsonstr)) -> Left jsonstr
        (Right Nothing) -> Right GetObjErrorNotFound
        (Left _) -> Right GetObjErrorDbError

getNewObjId :: Redis.Connection -> Handler Integer
getNewObjId dbh = do
  result <- liftIO $ Redis.runRedis dbh $ Redis.incr "counter_objid"
  case result of
    (Right val) -> return val
    _ -> throwError $ err404 { errBody = "No such object" }
