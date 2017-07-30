module Conveyor.Responsable
  ( class Responsable, encodeBody, statusCode
  , Result, result
  , ErrorMsg, errorMsg
  , StatusOnly, statusOnly
  , respond
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (encodeJSON)
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (HTTP, Response, responseAsStream, setHeader, setStatusCode)
import Node.Stream (end, writeString)



newtype Result r = Result { status :: Int, body :: r }

newtype ErrorMsg = ErrorMsg { status :: Int, message :: String }

newtype StatusOnly = StatusOnly Int



class Responsable r where
  encodeBody :: r -> String
  statusCode :: r -> Int



instance responsableResult :: Encode r => Responsable (Result r) where
  statusCode (Result r) = r.status
  encodeBody (Result r) = encodeJSON r.body



instance responsableErrorMsg :: Responsable ErrorMsg where
  statusCode (ErrorMsg r) = r.status
  encodeBody (ErrorMsg r) = "{ \"message\": \"" <> r.message <> "\" }"



instance responsableStatusOnly :: Responsable StatusOnly where
  statusCode (StatusOnly i) = i
  encodeBody _ = ""



result :: forall r. Encode r => Int -> r -> Result r
result status body = Result { status, body }



errorMsg :: Int -> String -> ErrorMsg
errorMsg status message = ErrorMsg { status, message }



statusOnly :: Int -> StatusOnly
statusOnly = StatusOnly



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
