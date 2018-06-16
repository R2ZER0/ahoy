{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Obj
  ( ObjId
  , ObjLookupError
  , objLookup
  , getObjId
  , objHasType
  , emptyCollection
  , emptyCollectionWithId
  , collectionFromObj
  , Collection ()
  , objFromCollection
  , collectionInsert
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as Json
import qualified Data.ByteString            as BSS
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Prelude                    hiding (otherwise)
import Data.Vector as Vec

type ObjId = T.Text

data ObjLookupError = ObjLookupErrorNotExists
                    | ObjLookupErrorNotObj
                    | ObjLookupErrorWrongDataType
                    | ObjLookupErrorNone
                    deriving (Show, Eq)

data ChainableLookup = Result Json.Value | Error ObjLookupError

otherwise :: ChainableLookup -> ChainableLookup -> ChainableLookup
(Error _) `otherwise` (Error err) = Error err
(Error _) `otherwise` (Result result) = Result result
(Result result) `otherwise` _ = Result result

objLookup :: T.Text -> Json.Value -> Either ObjLookupError Json.Value
objLookup param (Json.Object obj) = case Map.lookup param obj of
  (Just val) -> Right val
  Nothing    -> Left ObjLookupErrorNotExists
objLookup _ _                     = Left ObjLookupErrorNotObj

objLookupFirst :: [T.Text] -> Json.Value -> Either ObjLookupError Json.Value
objLookupFirst (param:params) o@(Json.Object hm) = case Map.lookup param hm of
  (Just val) -> Right val
  Nothing    -> objLookupFirst params o
objLookupFirst [] (Json.Object _) = Left ObjLookupErrorNotExists
objLookupFirst _ _ = Left ObjLookupErrorNotObj

objLookupChainable :: T.Text -> Json.Value -> ChainableLookup
objLookupChainable param (Json.Object obj) = case Map.lookup param obj of
  (Just val) -> Result val
  Nothing    -> Error ObjLookupErrorNotExists
objLookupChainable _ _ = Error ObjLookupErrorNotObj

getObjId :: Json.Value -> Either ObjLookupError ObjId
getObjId o@(Json.Object obj) = extract $ (objLookupChainable "id" o) `otherwise` (objLookupChainable "@id" o)
  where
    extract (Result (Json.String idstr)) = Right idstr
    extract (Result _)                   = Left ObjLookupErrorWrongDataType
    extract (Error err)                  = Left err


data Collection = CollectionInst Json.Value

objHasType :: T.Text -> Json.Value -> Bool
objHasType typeToCheck (Json.Object hm) = case Map.lookup "type" hm of
  (Just (Json.String typeStr)) -> typeStr == typeToCheck
  Nothing                      -> False

collectionFromObj :: Json.Value -> Maybe Collection
collectionFromObj o@(Json.Object hm) =
  if objHasType "Collection" o then Just $ CollectionInst objWithItems else Nothing
  where
    objWithItems = Json.Object $ Map.insertWith (\new old -> old) "items" (Json.toJSON ([] :: [Json.Value])) hm

objFromCollection :: Collection -> Json.Value
objFromCollection (CollectionInst obj) = obj

emptyCollection = CollectionInst $ Json.object
  [ "@context" .= Json.String "https://www.w3.org/ns/activitystreams"
  , "type" .= Json.String "Collection"
  , "items" .= ([] :: [Json.Value])
  ]
  
emptyCollectionWithId id = CollectionInst $ Json.object
  [ "@context" .= Json.String "https://www.w3.org/ns/activitystreams"
  , "id" .= Json.String id
  , "type" .= Json.String "Collection"
  , "items" .= ([] :: [Json.Value])
  ]

collectionInsert :: Collection -> Json.Value -> Collection
collectionInsert (CollectionInst (Json.Object hm)) o@(Json.Object _) =
  (CollectionInst (Json.Object (Map.adjust (\(Json.Array v) -> Json.Array (Vec.cons o v)) "items" hm)))
