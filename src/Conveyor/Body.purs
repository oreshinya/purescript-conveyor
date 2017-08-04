module Conveyor.Body where

import Data.Newtype (class Newtype)

newtype Body a = Body a

derive instance newtypeBody :: Newtype (Body a) _
