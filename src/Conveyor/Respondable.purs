module Conveyor.Respondable where

import Control.Monad.Eff.Exception (Error)
import Conveyor.Types (Responder)



class Respondable r where
  toResponder :: r -> Responder



class Respondable r <= RespondableError r where
  fromError :: Error -> r
