module Conveyor.Authenticatable where

import Control.Monad.Aff (Aff)
import Conveyor.Argument (Context, RawData)
import Conveyor.Respondable (class Respondable)
import Data.Either (Either)



newtype Auth a = Auth a

class Respondable r <= Authenticatable c e r a where
  authenticate :: Context c -> RawData -> Aff e (Either r a)
