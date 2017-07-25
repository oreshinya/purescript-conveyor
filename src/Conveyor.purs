module Conveyor (run) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Node.HTTP (HTTP, ListenOptions, createServer, listen, requestMethod, requestURL)
import Node.Stdout (log)
import Conveyor.Internal (respond)
import Conveyor.Responsable (ErrorMsg(..))
import Conveyor.Servable (class Servable, serve)



run :: forall e s. Servable e s => ListenOptions -> s -> Eff (http :: HTTP | e) Unit
run opts server = do
  server' <- createServer \req res ->
    let url = drop 1 $ requestURL req
        method = requestMethod req
     in case method of
          "POST" ->
            case serve server req res url of
              Just s -> s
              Nothing -> respond res $ ErrorMsg { status: 400, message: "No such route" }
          _ -> respond res $ ErrorMsg { status: 400, message: "HTTP method is not POST" }
  listen server' opts $ unsafeCoerceEff $ logListening opts



logListening :: forall e. ListenOptions -> Eff (console :: CONSOLE | e) Unit
logListening { port } = log $ "Listening on port " <> show port <> "."
