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
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import qualified Data.Aeson                 as Json
import qualified Data.ByteString            as BSS
import qualified Data.HashMap.Strict        as HashMap
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE

type ObjId = BSS.ByteString

data ObjLookupError = ObjLookupErrorNotExists
                    | ObjLookupErrorNotObj
                    | ObjLookupErrorNone
                    deriving (Show)

instance Monoid ObjLookupError where
  mappend err _ = err
  mempty = ObjLookupErrorNone

type ObjLookupMonad = ExceptT ObjLookupError IO


objLookup :: T.Text -> Json.Value -> ObjLookupMonad Json.Value
objLookup param (Json.Object obj) = case HashMap.lookup param obj of
  (Just val) -> return val
  Nothing    -> throwError ObjLookupErrorNotExists
objLookup _ _                     = throwError ObjLookupErrorNotObj


getObjId :: Json.Value -> ObjLookupMonad ObjId
getObjId o@(Json.Object obj) = do
  idVal <- (objLookup "id" o) <|> (objLookup "@id" o)
  case idVal of
    (Json.String idstr) -> return (TE.encodeUtf8 idstr)
    _                   -> throwError ObjLookupErrorNotObj
  where

