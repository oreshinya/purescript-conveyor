module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except (throwError)
import Conveyor (run)
import Conveyor.Handler (Handler)
import Conveyor.Responsable (Result(..))
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP, ListenOptions)
import Node.Process (PROCESS, lookupEnv)



newtype MyJson = MyJson { fuck :: String }

derive instance genericMyJson :: Generic MyJson _

instance encodeMyJson :: Encode MyJson where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

newtype YourJson = YourJson { yours :: String }

derive instance genericYourJson :: Generic YourJson _

instance encodeYourJson :: Encode YourJson where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

newtype Blog = Blog { title :: String, content :: String }

derive instance genericBlog :: Generic Blog _

instance decodeBlog :: Decode Blog where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }



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



errorTest :: forall e. Handler (console :: CONSOLE | e) (Result YourJson)
errorTest = do
  liftEff $ log "foo"
  throwError $ error ""



createBlog :: forall e. Blog -> Handler e (Result MyJson)
createBlog (Blog b) = pure $ Result
  { status: 200
  , body: Just $ MyJson { fuck: "title: " <> b.title <> ", content: " <> b.content <> " requested." }
  }



main :: forall e. Eff (process :: PROCESS, console :: CONSOLE, exception :: EXCEPTION, ref :: REF, http :: HTTP | e) Unit
main = do
  config <- getConfig
  run config { errorTest, createBlog }
