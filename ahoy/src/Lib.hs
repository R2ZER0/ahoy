{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    ) where

import GHC.Exts
import Data.Aeson
import qualified Data.Aeson as A
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type API = "outbox" :> Post '[JSON] A.Value
      :<|> "inbox"  :> Get  '[JSON] A.Value

startApp :: IO ()
startApp = do
  putStrLn "Starting on port 1234"
  run 1234 $ serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = outboxPost
    :<|> inboxGet

outboxPost = return (A.String "You did it!")

inboxGet = return (Object $ fromList [ ("thing", A.String "yes")
                                     , ("isa", A.Number 3)
                                     ])
