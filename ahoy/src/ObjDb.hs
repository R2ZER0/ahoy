{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module ObjDb
  ( PutObjError
  , putObjIntoDb
  , GetObjError
  , getObjFromDb
  , hostPrefix  -- TODO use config or something
  ) where

import           Control.Monad.Except
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Aeson                 as Json
import qualified Data.ByteString            as BSS
import qualified Data.ByteString.Char8      as BCS
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text.Encoding         as TE
import qualified Database.Redis             as Redis
import           Obj

hostPrefix = "http://localhost:1234/obj/"

str2redis :: String -> BSS.ByteString
str2redis str = BS.toStrict $ BC.pack $ str

data PutObjError = PutObjErrorId ObjLookupError
                 | PutObjErrorCantGenerateId
                 | PutObjErrorDbError
                 | PutObjErrorBadType
                 deriving (Show)

putObjIntoDb :: Redis.Connection -> Json.Value -> IO (Either PutObjError Json.Value)
putObjIntoDb dbh o@(Json.Object _) = withId o (getObjId o)
    where
      withId :: Json.Value -> Either ObjLookupError ObjId -> IO (Either PutObjError Json.Value)
      withId o (Right objid) = actuallyPutIntoDb o objid
      withId o (Left _) = do
        newObjIdM <- getNewObjId dbh
        withNewId o newObjIdM

      withNewId :: Json.Value -> (Maybe ObjId) -> IO (Either PutObjError Json.Value)
      withNewId o Nothing = return $ Left PutObjErrorCantGenerateId
      withNewId o@(Json.Object obj) (Just newObjId) = do
        let objHashWithId = HashMap.insert "id" (Json.toJSON $ TE.decodeUtf8 newObjId) obj
        actuallyPutIntoDb (Json.Object objHashWithId) newObjId

      actuallyPutIntoDb :: Json.Value -> ObjId -> IO (Either PutObjError Json.Value)
      actuallyPutIntoDb o@(Json.Object _) id = do
        result <- Redis.runRedis dbh $
          Redis.set (BS.toStrict (BC.pack ("obj:"++(show id))))
                    (BS.toStrict (Json.encode o))
        return $ doneRedis o result
      actuallyPutIntoDb _ _ = return (Left PutObjErrorBadType)

      doneRedis :: Json.Value -> Either Redis.Reply Redis.Status -> Either PutObjError Json.Value
      doneRedis o (Right Redis.Ok) = Right o
      doneRedis o _                = Left PutObjErrorDbError



data GetObjError = GetObjErrorNotFound
                 | GetObjErrorBadDatatype
                 | GetObjErrorCantDecode
                 | GetObjErrorDbError
                 deriving (Show, Eq)

getObjFromDb :: Redis.Connection -> ObjId -> IO (Either Json.Value GetObjError)
getObjFromDb dbh objid = do
  redisResult <- Redis.runRedis dbh $ Redis.get (str2redis ("obj:" ++ (show objid)))
  return $ case redisResult of
    (Right (Just jsonstr)) ->
      case Json.decode $ BS.fromStrict jsonstr of
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