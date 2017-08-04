module Conveyor
  ( run
  , runWithContext
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Conveyor.Responsable (errorMsg, respond)
import Conveyor.Servable (class Servable, serve)
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Node.HTTP (HTTP, ListenOptions, createServer, listen, requestURL)
import Node.Stdout (log)



run :: forall e s. Servable Unit e s => s -> ListenOptions -> Eff (http :: HTTP | e) Unit
run = runWithContext unit



runWithContext :: forall c e s. Servable c e s => c -> s -> ListenOptions -> Eff (http :: HTTP | e) Unit
runWithContext ctx server opts = do
  server' <- createServer \req res ->
    let url = drop 1 $ requestURL req
     in case serve ctx server req res url of
          Just s -> s
          Nothing -> respond res $ errorMsg 404 "No such route"
  listen server' opts $ unsafeCoerceEff $ logListening opts



logListening :: forall e. ListenOptions -> Eff (console :: CONSOLE | e) Unit
logListening { port } = log $ "Listening on port " <> show port <> "."
