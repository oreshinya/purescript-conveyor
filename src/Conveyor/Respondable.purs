module Conveyor.Respondable
  ( class Respondable, encodeBody, statusCode, systemError
  , ConveyorError(..)
  , respond
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (HTTP, Response, responseAsStream, setHeader, setStatusCode)
import Node.Stream (end, writeString)



data ConveyorError = ConveyorError Int String



class Respondable r where
  encodeBody :: r -> String
  statusCode :: r -> Int
  systemError :: Int -> String -> r



instance respondableConveyorError :: Respondable ConveyorError where
  statusCode (ConveyorError status _) = status
  encodeBody (ConveyorError _ message) = "{ \"message\": \"" <> message <> "\" }"
  systemError = ConveyorError



respond :: forall e r.
           Respondable r =>
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
