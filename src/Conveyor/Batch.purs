module Conveyor.Batch where

import Conveyor.Respondable (Responder(..))
import Data.Foreign (Foreign, toForeign)



newtype Batch s = Batch s

type BatchParams =
  Array
    { path :: String
    , body :: Foreign
    }



batchResponder :: Array Responder -> Responder
batchResponder rs =
  Responder
    { contentType: "application/json; charset=utf-8"
    , code: 200
    , body: toForeign rs
    }
