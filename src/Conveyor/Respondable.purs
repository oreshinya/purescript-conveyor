module Conveyor.Respondable where

import Conveyor.Types (Responder)
import Effect.Exception (Error)



class Respondable r where
  toResponder :: r -> Responder



class Respondable r <= RespondableError r where
  fromError :: Error -> r
