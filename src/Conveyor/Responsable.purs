module Conveyor.Responsable
  ( class Responsable, encodeBody, statusCode
  , Result(..)
  , ErrorMsg(..)
  , respond
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (encodeJSON)
import Data.Maybe (Maybe, maybe)
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (HTTP, Response, responseAsStream, setHeader, setStatusCode)
import Node.Stream (end, writeString)



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



respond :: forall e r.
           Responsable r =>
           Response ->
           r ->
           Eff (http :: HTTP | e) Unit
respond res r =
  let writable = responseAsStream res
   in do
     setHeader res "Content-Type" "application/json"
     setStatusCode res $ statusCode r
     void $ writeString writable UTF8 (encodeBody r) $ pure unit
     end writable $ pure unit
