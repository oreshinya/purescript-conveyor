module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Node.HTTP (HTTP, ListenOptions)
import Node.Process (PROCESS, lookupEnv)
import Data.Generic.Rep (class Generic)
import Conveyor (run)
import Conveyor.Handler (Handler)
import Conveyor.Responsable (Result, result)



newtype MyJson = MyJson { fuck :: String }

derive instance genericMyJson :: Generic MyJson _

instance encodeMyJson :: Encode MyJson where
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



createBlog :: forall e. Blog -> Handler e (Result MyJson)
createBlog (Blog b) = pure $ result 200 $ Just $ MyJson { fuck: "title: " <> b.title <> ", content: " <> b.content <> " requested." }



main :: forall e. Eff (process :: PROCESS, console :: CONSOLE, exception :: EXCEPTION, ref :: REF, http :: HTTP | e) Unit
main = do
  config <- getConfig
  run config { createBlog }
