{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    ) where

import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Aeson                 as Json
import qualified Data.ByteString            as BSS
import qualified Data.ByteString.Char8      as BCS
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.HashMap.Strict        (lookup)
import qualified Data.Scientific            as Scientific
import qualified Data.Text                  as Text
import qualified Database.Redis             as Redis
import           GHC.Exts
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Obj
import           ObjDb
import           Servant
import           Servant.Server             (err400, err404, err500)



type API = "outbox" :> ReqBody '[JSON] Json.Value :> Post '[JSON] Json.Value
      :<|> "inbox"  :> Get  '[JSON] Json.Value
      :<|> "obj"    :> Capture "objid" Text.Text :> Get  '[JSON] Json.Value

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
        :<|> objGet
    where
      outboxPost o@(Json.Object obj) = do
        result <- liftIO $ putObjIntoDb dbh (Json.Object obj)
        case result of
          (Left err) -> throwError $ err400 {
            errBody = BC.concat ["Object error ",  Json.encode o, ": ", (BC.pack (show err))]
          }
          (Right objWithId) -> do
            liftIO $ BC.putStrLn $ "Posted: " `BS.append` (Json.encode objWithId)
            return $ Json.Object $ fromList [ ("success", Json.String "You did it!")
                                   , ("posted",  objWithId)
                                   ]
      outboxPost _ = do
        throwError $ err400 { errBody = "Must be JSON object" }

      inboxGet = objGet "2"

      objGet textobjid = do
        let objid = Text.append hostPrefix $ textobjid
        result <- liftIO $ getObjFromDb dbh objid
        case result of
          (Right obj) -> return obj
          (Left err) -> throwError $ err404 {
            errBody = BS.concat ["Can't get object ", BC.pack ((show objid) ++ ": " ++ (show err))]
          }
