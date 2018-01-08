module Conveyor.Readable
  ( class Readable, readBody
  , decodeBody
  ) where

import Prelude

import Control.Monad.Except (throwError)
import Data.Array (head)
import Data.Foreign (F, ForeignError(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.StrMap (lookup)
import Data.String (Pattern(..), split)
import Node.HTTP (Request, requestHeaders)



class Readable a where
  readBody :: String -> F a



decodeBody :: forall a. Readable a => Request -> String -> F a
decodeBody req rawBody =
  case lookup "content-type" (requestHeaders req) >>= parseMediaType of
    Just mediaType | mediaType == applicationJSON -> readBody rawBody
    _ -> throwError $ singleton $ ForeignError "Received unpermitted Content-Type."



parseMediaType :: String -> Maybe MediaType
parseMediaType = split (Pattern ";") >>> head >>> map MediaType
