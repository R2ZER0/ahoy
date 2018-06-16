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
  , getCollectionFromDb
  , getCollectionFromDbOrEmpty
  , addToDbCollection
  ) where

import           Control.Monad.Except
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Aeson                 as Json
import qualified Data.ByteString            as BSS
import qualified Data.ByteString.Char8      as BCS
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.HashMap.Strict        as Map
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as TE
import qualified Database.Redis             as Redis
import           Obj

hostPrefix :: Text.Text
hostPrefix = "http://localhost:1234/obj/"

str2redis :: String -> BSS.ByteString
str2redis str = BS.toStrict $ BC.pack $ str

data ObjMapError a = ObjMapErrorGet GetObjError
                   | ObjMapErrorPut PutObjError
                   | ObjMapErrorMap a
                   deriving (Show, Eq)

data UpdateObjError = UpdateObjErrorGet GetObjError
                    | UpdateObjErrorPut PutObjError
                    | UpdateObjErrorWrongType
                    | UpdateObjErrorCantParse
                    | UpdateObjErrorUnknown
                    deriving (Show, Eq)

data PutObjError = PutObjErrorId ObjLookupError
                 | PutObjErrorCantGenerateId
                 | PutObjErrorDbError
                 | PutObjErrorBadType
                 deriving (Show, Eq)

data GetObjError = GetObjErrorNotFound
                 | GetObjErrorBadDatatype
                 | GetObjErrorCantDecode
                 | GetObjErrorCantParse
                 | GetObjErrorDbError
                 deriving (Show, Eq)

mapDb :: (Json.Value -> Either a Json.Value) -> Redis.Connection -> ObjId -> IO (Either (ObjMapError a) Json.Value)
mapDb f dbh objId = do
  objResult <- getObjFromDb dbh objId
  case objResult of
    Left getErr -> return $ Left $ ObjMapErrorGet getErr
    Right obj -> do
      let mapResult = f obj
      case mapResult of
        Left err -> return $ Left $ ObjMapErrorMap err
        Right newObj -> do
          putResult <- putObjIntoDb dbh newObj
          case putResult of
            Left putErr -> return $ Left $ ObjMapErrorPut putErr
            Right putObj -> return $ Right putObj

getCollectionFromDb :: Redis.Connection -> ObjId -> IO (Either GetObjError Collection)
getCollectionFromDb dbh collId = do
  objResult <- getObjFromDb dbh collId
  return $ case objResult of
    Left getErr -> Left getErr
    Right obj -> case collectionFromObj obj of
      Just coll -> Right coll
      Nothing -> Left GetObjErrorCantParse

getCollectionFromDbOrEmpty :: Redis.Connection -> ObjId -> IO Collection
getCollectionFromDbOrEmpty dbh collId = do
  result <- getCollectionFromDb dbh collId
  return $ case result of
    Left _ -> emptyCollection
    Right coll -> coll

addToDbCollection :: Redis.Connection -> ObjId -> Json.Value -> IO (Either UpdateObjError Json.Value)
addToDbCollection dbh collId objToPut = do
  result <- mapDb updateF dbh collId
  return $ case result of
    Left (ObjMapErrorMap GetObjErrorCantParse) -> Left UpdateObjErrorCantParse
    Left (ObjMapErrorMap _) -> Left UpdateObjErrorUnknown
    Left (ObjMapErrorPut putErr) -> Left (UpdateObjErrorPut putErr)
    Left (ObjMapErrorGet getErr) -> Left (UpdateObjErrorGet getErr)
    Right value -> Right value
  where
    updateF :: Json.Value -> Either GetObjError Json.Value
    updateF obj = case collectionFromObj obj of
        Nothing -> Left GetObjErrorCantParse
        Just coll -> Right $ objFromCollection (collectionInsert coll objToPut) 

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
        let objHashWithId = Map.insert "id" (Json.toJSON newObjId) obj
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




getObjFromDb :: Redis.Connection -> ObjId -> IO (Either GetObjError Json.Value)
getObjFromDb dbh objid = do
  redisResult <- Redis.runRedis dbh $ Redis.get (str2redis ("obj:" ++ (show objid)))
  return $ case redisResult of
    (Right (Just jsonstr)) ->
      case Json.decode $ BS.fromStrict jsonstr of
        (Just jsonval) -> Right jsonval
        Nothing        -> Left GetObjErrorCantDecode
    (Right Nothing) -> Left GetObjErrorNotFound
    (Left _) -> Left GetObjErrorDbError

getNewObjId :: Redis.Connection -> IO (Maybe ObjId)
getNewObjId dbh = do
  result <- liftIO $ Redis.runRedis dbh $ Redis.incr "counter_objid"
  return $ case result of
    (Right val) -> Just (Text.append hostPrefix (Text.pack $ show $ val))
    (Left _)    -> Nothing
