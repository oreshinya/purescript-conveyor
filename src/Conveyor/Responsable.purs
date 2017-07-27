module Conveyor.Responsable
  ( class Responsable, encodeBody, statusCode
  , Result, result
  , ErrorMsg, errorMsg
  ) where

import Prelude
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (encodeJSON)
import Data.Maybe (Maybe, maybe)



newtype Result r = Result { status :: Int, body :: Maybe r }

newtype ErrorMsg = ErrorMsg { status :: Int, message :: String }



class Responsable r where
  encodeBody :: r -> String
  statusCode :: r -> Int



instance responsableResult :: Encode r => Responsable (Result r) where
  statusCode (Result r) = r.status
  encodeBody (Result r) = maybe "" encodeJSON r.body



instance responsableErrorMsg :: Responsable ErrorMsg where
  statusCode (ErrorMsg r) = r.status
  encodeBody (ErrorMsg r) = "{ \"message\": \"" <> r.message <> "\" }"



result :: forall r. Encode r => Int -> Maybe r -> Result r
result status body = Result { status, body }



errorMsg :: Int -> String -> ErrorMsg
errorMsg status message = ErrorMsg { status, message }
