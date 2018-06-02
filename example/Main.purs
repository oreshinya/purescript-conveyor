module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Control.Monad.Except (throwError)
import Conveyor (handlerWithExtraData)
import Conveyor.Handler (Handler, askExtra, askRaw)
import Conveyor.Logger (withLogger)
import Conveyor.Respondable (class Respondable, class RespondableError)
import Conveyor.Types (Body(..), Batch(..), Responder(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.HTTP (ListenOptions, createServer, listen)
import Node.Process (lookupEnv)
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



getHostname :: Effect String
getHostname = pure "0.0.0.0"



getPort :: Effect Int
getPort = do
  portStr <- lookupEnv "PORT"
  pure $ case (map fromString portStr) of
    Just (Just i) -> i
    _ -> 3000



getBacklog :: Effect (Maybe Int)
getBacklog = pure Nothing



getConfig :: Effect ListenOptions
getConfig = do
  hostname <- getHostname
  port <- getPort
  backlog <- getBacklog
  pure { hostname, port, backlog }



errorTest :: Handler Int (Result YourJson)
errorTest = do
  liftEffect $ log "foo"
  extra <- askExtra
  liftEffect $ log $ show extra
  raw <- askRaw
  liftEffect $ log raw.path
  liftEffect $ log raw.rawBody
  throwError $ error "This is test error!!!"



createBlog :: Body Blog -> Handler Int (Result MyJson)
createBlog (Body b) = pure $ Success
  { status: 200
  , body: { fuck: "title: " <> b.title <> ", content: " <> b.content <> " requested." }
  }



main :: Effect Unit
main = do
  config <- getConfig
  server <- createServer $ handlerWithExtraData 777 $ withLogger $ Batch { errorTest, createBlog }
  listen server config $ pure unit
