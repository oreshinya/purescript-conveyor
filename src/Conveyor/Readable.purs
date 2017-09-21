module Conveyor.Readable where

import Data.Foreign (F)

class Readable a where
  readBody :: String -> F a
