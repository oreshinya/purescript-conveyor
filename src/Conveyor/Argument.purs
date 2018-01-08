module Conveyor.Argument where

import Node.HTTP (Request, Response)



newtype Body a = Body a

newtype Context a = Context a

newtype RawData = RawData
  { req :: Request
  , res :: Response
  , path :: String
  , rawBody :: String
  }
