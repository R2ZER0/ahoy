{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Obj
  ( ObjId
  , lookupObj
  , ObjIdError
  , getObjId
  ) where

import qualified Data.Aeson          as Json
import qualified Data.ByteString     as BSS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE

type ObjId = BSS.ByteString

lookupObj :: T.Text -> Json.Value -> Maybe Json.Value
lookupObj param (Json.Object obj) = HashMap.lookup param obj
lookupObj _ _                     = Nothing

data ObjIdError = ObjIdErrorNoId | ObjIdErrorBadId deriving (Show, Read)

getObjId :: Json.Value -> Either ObjId ObjIdError
getObjId o@(Json.Object obj) =
  case lookupObj "id" o of
    (Just idval) -> case idval of
      (Json.String idstr) -> Left (TE.encodeUtf8 idstr)
      _                   -> Right ObjIdErrorBadId
    _ -> Right ObjIdErrorNoId
