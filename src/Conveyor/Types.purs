module Conveyor.Types where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Foreign (Foreign)
import Node.HTTP (Request, Response)



newtype Body a = Body a



type RawData =
  { req :: Request
  , res :: Response
  , path :: String
  , rawBody :: String
  }



newtype Batch server = Batch server



data LogInfo
  = Start
  | End Responder



data Logger eff server =
  Logger (RawData -> LogInfo -> Aff eff Unit) server



newtype Responder = Responder
  { contentType :: String
  , code :: Int
  , body :: Foreign
  }
