module Conveyor (run) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Node.HTTP (HTTP, ListenOptions, createServer, listen, requestURL)
import Node.Stdout (log)
import Conveyor.Responsable (errorMsg, respond)
import Conveyor.Servable (class Servable, serve)



run :: forall e s. Servable e s => ListenOptions -> s -> Eff (http :: HTTP | e) Unit
run opts server = do
  server' <- createServer \req res ->
    let url = drop 1 $ requestURL req
     in case serve server req res url of
          Just s -> s
          Nothing -> respond res $ errorMsg 404 "No such route"
  listen server' opts $ unsafeCoerceEff $ logListening opts



logListening :: forall e. ListenOptions -> Eff (console :: CONSOLE | e) Unit
logListening { port } = log $ "Listening on port " <> show port <> "."
