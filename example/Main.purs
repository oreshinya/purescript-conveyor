module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Except (throwError)
import Conveyor (handlerWithContext)
import Conveyor.Argument (Body(..), Context(..), RawData(..))
import Conveyor.Batch (Batch(..))
import Conveyor.Logger (withLogger)
import Conveyor.Respondable (class Respondable, class RespondableError, Responder(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP, ListenOptions, createServer, listen)
import Node.Process (PROCESS, lookupEnv)
import Simple.JSON (class WriteForeign, write)



data Result r
  = Success { status :: Int, body :: r }
  | Failure { status :: Int, message :: String }

type MyJson = { fuck :: String }

type YourJson = { yours :: String }

type Blog = { title :: String, content :: String }



instance respondableResult :: WriteForeign r => Respondable (Result r) where
  toResponder (Success s) =
    Responder
      { contentType: "application/json; charset=utf-8"
      , code: s.status
      , body: write s.body
      }
  toResponder (Failure f) =
    Responder
      { contentType: "application/json; charset=utf-8"
      , code: f.status
      , body: write { messages: [ f.message ] }
      }

instance respondableErrorResult :: WriteForeign r => RespondableError (Result r) where
  fromError _ = Failure { status: 500, message: "Internal server error ;)" }



getHostname :: forall e. Eff e String
getHostname = pure "0.0.0.0"



getPort :: forall e. Eff (process :: PROCESS | e) Int
getPort = do
  portStr <- lookupEnv "PORT"
  pure $ case (map fromString portStr) of
    Just (Just i) -> i
    _ -> 3000



getBacklog :: forall e. Eff e (Maybe Int)
getBacklog = pure Nothing



getConfig :: forall e. Eff (process :: PROCESS | e) ListenOptions
getConfig = do
  hostname <- getHostname
  port <- getPort
  backlog <- getBacklog
  pure { hostname, port, backlog }



rawDataTest :: forall e. RawData -> Aff e (Result YourJson)
rawDataTest (RawData rd) =
  pure $ Success { status: 200, body: { yours: rd.rawBody } }



contextTest :: forall e. Context Int -> Aff e (Result YourJson)
contextTest (Context i) =
  pure $ Success { status: 200, body: { yours: show i } }



errorTest :: forall e. Aff (console :: CONSOLE | e) (Result YourJson)
errorTest = do
  liftEff $ log "foo"
  throwError $ error ""



createBlog :: forall e. Body Blog -> Aff e (Result MyJson)
createBlog (Body b) = pure $ Success
  { status: 200
  , body: { fuck: "title: " <> b.title <> ", content: " <> b.content <> " requested." }
  }



main :: forall e. Eff (now :: NOW, process :: PROCESS, console :: CONSOLE, http :: HTTP | e) Unit
main = do
  config <- getConfig
  server <- createServer $ handlerWithContext 777 $ withLogger $ Batch { contextTest, errorTest, rawDataTest, createBlog }
  listen server config $ pure unit
