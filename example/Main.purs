module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (ExceptT, throwError)
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Node.HTTP (HTTP)
import Node.Process (PROCESS, lookupEnv)
import Data.Generic.Rep (class Generic)
import Conveyor (Config(..), Context, Break(..), Result(..), Router, App, (:>), (</>), route, run, defaultApp, parseBody)



newtype MyJson = MyJson { fuck :: String }

derive instance genericMyJson :: Generic MyJson _

instance encodeMyJson :: Encode MyJson where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

newtype Comment = Comment { content :: String }

derive instance genericComment :: Generic Comment _

instance decodeComment :: Decode Comment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

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



getConfig :: forall e. Eff (process :: PROCESS | e) Config
getConfig = do
  hostname <- getHostname
  port <- getPort
  backlog <- getBacklog
  pure $ Config { hostname, port, backlog }




handleError :: forall e. Context -> ExceptT Break (Eff e) (Result MyJson)
handleError ctx = do
  void $ throwError $ Break { status: 500, message: Just "Failed request." }
  (Comment comment) <- parseBody ctx
  pure $ Result { status: 200, body: Just (MyJson { fuck: comment.content }) }



createBlog :: forall e. Context -> ExceptT Break (Eff e) (Result MyJson)
createBlog ctx = do
  (Blog blog) <- parseBody ctx
  pure $ Result
    { status: 200
    , body: Just (MyJson { fuck: "title: " <> blog.title <> ", content: " <> blog.content <> " requested." })
    }



router :: forall e. Router Context e MyJson
router = route $
  "/v1" </>
    [ "/handle_error" :> handleError
    , "/create_blog" :> createBlog
    ]
  <>
  "/v2" </>
    [ "/handle_error" :> handleError
    , "/create_blog" :> createBlog
    ]




app :: forall e. App Context e MyJson
app = defaultApp router



main :: forall e. Eff (process :: PROCESS, console :: CONSOLE, exception :: EXCEPTION, ref :: REF, http :: HTTP | e ) Unit
main = do
  config <- getConfig
  run config app
