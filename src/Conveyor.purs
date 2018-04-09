module Conveyor
  ( handler
  , handlerWithContext
  ) where

import Prelude

import Control.Monad.Aff (runAff_)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Conveyor.Argument (Context(..), RawData(..))
import Conveyor.Internal (logError)
import Conveyor.Respondable (conveyorError, send)
import Conveyor.Servable (class Servable, serve)
import Data.Either (Either(..))
import Data.String (drop)
import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP, Request, Response, requestAsStream, requestURL)
import Node.Stream (onDataString, onEnd, onError)



handler
  :: forall e s
   . Servable Unit (http :: HTTP | e) s
  => s
  -> (Request -> Response -> Eff (http :: HTTP | e) Unit)
handler = handlerWithContext unit



handlerWithContext
  :: forall c e s
   . Servable c (http :: HTTP | e) s
  => c
  -> s
  -> (Request -> Response -> Eff (http :: HTTP | e) Unit)
handlerWithContext ctx servable = \req res ->
  let path = drop 1 $ requestURL req
      readable = requestAsStream req

      callback (Left err) = onError' err
      callback (Right suc) = send res suc

      onDataString' ref chunk = readRef ref >>= writeRef ref <<< flip append chunk
      onError' err = do
        logError err
        send res $ conveyorError 500 "Internal Server Error"
      onEnd' ref = do
        rawBody <- readRef ref
        runAff_ callback $ unsafeCoerceAff $ serve servable (Context ctx) $ RawData { req, res, path, rawBody }

   in unsafeCoerceEff do
      ref <- newRef ""
      onDataString readable UTF8 $ onDataString' ref
      onError readable onError'
      onEnd readable $ onEnd' ref
