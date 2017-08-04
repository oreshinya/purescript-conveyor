module Conveyor.Context where

import Data.Newtype (class Newtype)

newtype Context a = Context a

derive instance newtypeContext :: Newtype (Context a) _
