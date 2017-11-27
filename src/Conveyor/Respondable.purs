module Conveyor.Respondable
  ( class Respondable, encodeBody, contentType, statusCode, systemError
  , ConveyorError(..)
  , respond
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (HTTP, Response, responseAsStream, setHeader, setStatusCode)
import Node.Stream (end, writeString)



data ConveyorError = ConveyorError Int String



class Respondable r where
  contentType :: r -> String
  encodeBody :: r -> String
  statusCode :: r -> Int
  systemError :: Error -> r



instance respondableConveyorError :: Respondable ConveyorError where
  contentType _ = "application/json"
  statusCode (ConveyorError status _) = status
  encodeBody (ConveyorError _ message) = "{ \"message\": \"" <> message <> "\" }"
  systemError = const $ ConveyorError 500 ""



respond
  :: forall e r
   . Respondable r
  => Response
  -> r
  -> Eff (http :: HTTP | e) Unit
respond res r =
  let writable = responseAsStream res
   in do
     setHeader res "Content-Type" $ contentType r
     setStatusCode res $ statusCode r
     void $ writeString writable UTF8 (encodeBody r) $ pure unit
     end writable $ pure unit
