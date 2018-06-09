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

type API = "outbox" :> ReqBody '[JSON] A.Value :> Post '[JSON] A.Value
      :<|> "inbox"  :> Get  '[JSON] A.Value

startApp :: IO ()
startApp = do
  dbh <- Redis.checkedConnect Redis.defaultConnectInfo
  Redis.runRedis dbh $ do
     Redis.set "hello" "hello"
     Redis.set "world" "world"
     hello <- Redis.get "hello"
     world <- Redis.get "world"
     liftIO $ print (hello,world)
  putStrLn "Starting on port 1234"
  run 1234 $ serve api (server dbh)

api :: Proxy API
api = Proxy

type ObjId = Integer

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

      inboxGet =
        return $ Object $ fromList [ ("thing", A.String "yes")
                                   , ("isa", A.Number 3)
                                   ]

lookupObj :: T.Text -> Value -> Maybe Value
lookupObj param (A.Object obj) = HashMap.lookup param obj
lookupObj _ _ = Nothing

getObjId :: Value -> Maybe ObjId
getObjId (A.Object obj) =
  case lookupObj "id" (Object obj) of
    (Just (A.Number idsci)) ->
      case Scientific.floatingOrInteger idsci of
        (Right idint) -> Just idint
        (Left _) -> Nothing
    _ -> Nothing

putObjIntoDb :: Redis.Connection -> A.Value -> Handler Value
putObjIntoDb dbh (Object obj) = do
  case getObjId (A.Object obj) of
    (Just objId) -> actuallyPutIntoDb (Object obj)
    Nothing -> do
        newObjId <- getNewObjId dbh
        let objWithId = HashMap.insert "id" (toJSON $ hostPrefix ++ (show newObjId)) obj
        actuallyPutIntoDb (Object objWithId)
    where
      actuallyPutIntoDb o@(Object _) =
        case getObjId o of
          (Just objId) -> do
            result <- liftIO $ Redis.runRedis dbh $
              Redis.set (BS.toStrict (BC.pack ("obj:"++(show objId))))
                        (BS.toStrict (A.encode o))
            return o
          Nothing -> throwError $ err400 { errBody = "attempt to put object with no id" }

str2redis :: String -> BSS.ByteString
str2redis str = BS.toStrict $ BC.pack $ str


getObjFromDb :: Redis.Connection -> ObjId -> Handler (Maybe Value)
getObjFromDb dbh objid = do
  redisResult <- liftIO $ redisResultDo
  return $ case redisResult of
    (Just jsonstr) -> A.decode $ BS.fromStrict jsonstr
    Nothing -> Nothing
  where
    redisResultDo :: IO (Maybe BSS.ByteString)
    redisResultDo = do
      result <- Redis.runRedis dbh $ Redis.get (str2redis ("obj:" ++ (show objid)))
      case result of
        (Left (Redis.SingleLine jsonstr)) -> do
          return (Just jsonstr)
        _ -> do
          return Nothing

getNewObjId :: Redis.Connection -> Handler Integer
getNewObjId dbh = do
  result <- liftIO $ Redis.runRedis dbh $ Redis.incr "counter_objid"
  case result of
    (Right val) -> return val
    _ -> throwError $ err404 { errBody = "No such object" }
