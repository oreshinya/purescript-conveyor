module Conveyor.Internal
  ( BatchParams
  , LProxy(..)
  , get
  , rowToList
  , decodeBody
  , logError
  , conveyorError
  , batchResponder
  , send
  ) where

import Prelude

import Conveyor.Types (Responder(..))
import Data.Array (head)
import Data.Either (Either(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Effect (Effect)
import Effect.Exception (Error, message, stack)
import Foreign (Foreign, ForeignError(..), MultipleErrors, unsafeToForeign)
import Foreign.Object (lookup)
import Global.Unsafe (unsafeStringify)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, requestHeaders, responseAsStream, setHeader, setStatusCode)
import Node.Stdout (log)
import Node.Stream (end, writeString)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons)
import Simple.JSON (class ReadForeign, readJSON)
import Type.Row (class RowToList, kind RowList)
import Unsafe.Coerce (unsafeCoerce)



type BatchParams =
  Array
    { path :: String
    , body :: Foreign
    }



data LProxy (l :: RowList) = LProxy



get
  :: forall l a r1 r2
   . IsSymbol l
  => Cons l a r1 r2
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



logError :: Error -> Effect Unit
logError err = log $ fromMaybe (message err) $ stack err



conveyorError :: Int -> String -> Responder
conveyorError code message =
  Responder
    { contentType: "application/json; charset=utf-8"
    , code
    , body: unsafeToForeign { message }
    }



batchResponder :: Array Responder -> Responder
batchResponder rs =
  Responder
    { contentType: "application/json; charset=utf-8"
    , code: 200
    , body: unsafeToForeign rs
    }



send
  :: Response
  -> Responder
  -> Effect Unit
send res (Responder r) = do
  setHeader res "Content-Type" r.contentType
  setStatusCode res r.code
  void $ writeString writable UTF8 (unsafeStringify r.body) $ pure unit
  end writable $ pure unit
  where
    writable = responseAsStream res
