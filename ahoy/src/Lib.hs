{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    ) where

import Obj
import ObjDb
import qualified Data.ByteString            as BSS
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson
import qualified Data.Aeson                 as A
import           Data.Aeson.TH
import           Data.HashMap.Strict        (lookup)
import qualified Data.Scientific            as Scientific
import qualified Data.Text                  as T
import           GHC.Exts
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server             (err400, err404, err500)
import qualified Data.ByteString.Char8      as BCS
import qualified Database.Redis             as Redis
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as BC



type API = "outbox" :> ReqBody '[JSON] A.Value :> Post '[JSON] A.Value
      :<|> "inbox"  :> Get  '[JSON] A.Value
      :<|> "obj"    :> Capture "objid" String :> Get  '[JSON] A.Value

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

      inboxGet = objGet "11"

      objGet textobjid = do
        let objid = BSS.append hostPrefix $ BCS.pack textobjid
        result <- liftIO $ getObjFromDb dbh objid
        case result of
          (Left obj) -> return obj
          (Right err) -> throwError $ err404 {
            errBody = BS.concat ["Can't get object ", BC.pack ((show objid) ++ ": " ++ (show err))]
          }
