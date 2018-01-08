module Conveyor.Internal
  ( LProxy(..)
  , get
  , rowToList
  , decodeBody
  ) where

import Prelude

import Data.Array (head)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(..), MultipleErrors)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.StrMap (lookup)
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Node.HTTP (Request, requestHeaders)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign, readJSON)
import Type.Row (class RowToList, kind RowList)
import Unsafe.Coerce (unsafeCoerce)



data LProxy (l :: RowList) = LProxy



get
  :: forall l a r1 r2
   . IsSymbol l
  => RowCons l a r1 r2
  => SProxy l
  -> Record r2
  -> a
get l r =
    fromMaybe'
      (\_ -> unsafeCrashWith ("unsafeGet: missing key " <> show s))
      (lookup s (unsafeCoerce r))
  where
    s = reflectSymbol l



rowToList :: forall proxy r l. RowToList r l => proxy r -> LProxy l
rowToList _ = LProxy



decodeBody :: forall a. ReadForeign a => Request -> String -> Either MultipleErrors a
decodeBody req rawBody =
  case lookup "content-type" (requestHeaders req) >>= parseMediaType of
    Just mediaType | mediaType == applicationJSON -> readJSON rawBody
    _ -> Left $ singleton $ ForeignError "Received unpermitted Content-Type."



parseMediaType :: String -> Maybe MediaType
parseMediaType = split (Pattern ";") >>> head >>> map MediaType
