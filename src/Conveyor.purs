module Conveyor
  ( handler
  , handlerWithExtraData
  ) where

import Prelude

import Conveyor.Internal (conveyorError, logError, send)
import Conveyor.Servable (class Servable, serve)
import Data.Either (Either(..))
import Data.String (drop)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Ref (new, read, modify_)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, requestAsStream, requestURL)
import Node.Stream (onDataString, onEnd, onError)



handler
  :: forall server
   . Servable Unit server
  => server
  -> (Request -> Response -> Effect Unit)
handler = handlerWithExtraData unit



handlerWithExtraData
  :: forall ex server
   . Servable ex server
  => ex
  -> server
  -> (Request -> Response -> Effect Unit)
handlerWithExtraData extraData server = \req res ->
  let path = drop 1 $ requestURL req
      readable = requestAsStream req

      callback (Left err) = onError' err
      callback (Right suc) = send res suc

      onDataString' ref chunk = modify_ (flip append chunk) ref
      onError' err = do
        logError err
        send res $ conveyorError 500 "Internal Server Error"
      onEnd' ref = do
        rawBody <- read ref
        runAff_ callback $ serve server extraData { req, res, path, rawBody }

   in do
      ref <- new ""
      onDataString readable UTF8 $ onDataString' ref
      onError readable onError'
      onEnd readable $ onEnd' ref
