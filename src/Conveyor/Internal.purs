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

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, message, stack)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Conveyor.Types (Responder(..))
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError(..), MultipleErrors, toForeign)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.StrMap (lookup)
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Global.Unsafe (unsafeStringify)
import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP, Request, Response, requestHeaders, responseAsStream, setHeader, setStatusCode)
import Node.Stdout (log)
import Node.Stream (end, writeString)
import Partial.Unsafe (unsafeCrashWith)
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



logError :: forall eff. Error -> Eff eff Unit
logError err = unsafeCoerceEff $ log $ fromMaybe (message err) $ stack err



conveyorError :: Int -> String -> Responder
conveyorError code message =
  Responder
    { contentType: "application/json; charset=utf-8"
    , code
    , body: toForeign { message }
    }



batchResponder :: Array Responder -> Responder
batchResponder rs =
  Responder
    { contentType: "application/json; charset=utf-8"
    , code: 200
    , body: toForeign rs
    }



send
  :: forall eff
   . Response
  -> Responder
  -> Eff (http :: HTTP | eff) Unit
send res (Responder r) = do
  setHeader res "Content-Type" r.contentType
  setStatusCode res r.code
  void $ writeString writable UTF8 (unsafeStringify r.body) $ pure unit
  end writable $ pure unit
  where
    writable = responseAsStream res
