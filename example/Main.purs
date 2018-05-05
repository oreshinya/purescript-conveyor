module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Except (throwError)
import Conveyor (handlerWithExtraData)
import Conveyor.Handler (Handler, askExtra, askRaw)
import Conveyor.Logger (withLogger)
import Conveyor.Respondable (class Respondable, class RespondableError)
import Conveyor.Types (Body(..), Batch(..), Responder(..))
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



getHostname :: forall eff. Eff eff String
getHostname = pure "0.0.0.0"



getPort :: forall eff. Eff (process :: PROCESS | eff) Int
getPort = do
  portStr <- lookupEnv "PORT"
  pure $ case (map fromString portStr) of
    Just (Just i) -> i
    _ -> 3000



getBacklog :: forall eff. Eff eff (Maybe Int)
getBacklog = pure Nothing



getConfig :: forall eff. Eff (process :: PROCESS | eff) ListenOptions
getConfig = do
  hostname <- getHostname
  port <- getPort
  backlog <- getBacklog
  pure { hostname, port, backlog }



errorTest :: forall eff. Handler Int (console :: CONSOLE | eff) (Result YourJson)
errorTest = do
  liftEff $ log "foo"
  extra <- askExtra
  liftEff $ log $ show extra
  raw <- askRaw
  liftEff $ log raw.path
  liftEff $ log raw.rawBody
  throwError $ error "This is test error!!!"



createBlog :: forall eff. Body Blog -> Handler Int eff (Result MyJson)
createBlog (Body b) = pure $ Success
  { status: 200
  , body: { fuck: "title: " <> b.title <> ", content: " <> b.content <> " requested." }
  }



main :: forall eff. Eff (now :: NOW, process :: PROCESS, console :: CONSOLE, http :: HTTP | eff) Unit
main = do
  config <- getConfig
  server <- createServer $ handlerWithExtraData 777 $ withLogger $ Batch { errorTest, createBlog }
  listen server config $ pure unit
