module Conveyor
  ( Config(..)
  , Context(..)
  , Result(..)
  , Failure(..)
  , Handler(..)
  , Router(..)
  , Before(..)
  , After(..)
  , App(..)
  , run
  , parseBody
  , handle
  , (:>)
  , defaultApp
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Except (ExceptT, runExceptT, runExcept, throwError)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (encodeJSON, decodeJSON)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.String (Pattern(..), split)
import Data.StrMap as SM
import Data.Tuple as T
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (HTTP, Request, Response, listen, createServer, requestAsStream, responseAsStream, setHeader, setStatusCode, requestURL, requestMethod, requestHeaders)
import Node.Stream (writeString, end, onDataString, onError, onEnd)


newtype Config
  = Config
    { hostname :: String
    , port :: Int
    , backlog :: Maybe Int
    }

newtype Context
  = Context
    { req :: Request
    , res :: Response
    , body :: Maybe String
    }

newtype Result r
  = Result
    { status :: Int
    , body :: Maybe r
    }

newtype Failure
  = Failure
    { status :: Int
    , message :: String
    }

newtype Handler c e r
  = Handler (c -> ExceptT Failure (Eff e) (Result r))

newtype Router c e r
  = Router (Array (Tuple String (Handler c e r)))

newtype Before e c
  = Before (Context -> ExceptT Failure (Eff e) c)

newtype After e r
  = After (Result r -> ExceptT Failure (Eff e) (Result r))

newtype App c e r
  = App
    { before :: Before e c
    , router :: Router c e r
    , after :: After e r
    }



run :: forall c e r.
       Encode r =>
       Config ->
       App c (console :: CONSOLE, exception :: EXCEPTION, http :: HTTP, ref :: REF | e) r ->
       Eff (console :: CONSOLE, exception :: EXCEPTION, http :: HTTP, ref :: REF | e) Unit
run (Config c) app = do
  server <- createServer $ serve app
  listen server c $ logListening c.port



logListening :: forall e. Int -> Eff (console :: CONSOLE | e) Unit
logListening port = log $ "Listening on port " <> show port <> "."



serve :: forall c e r.
         Encode r =>
         App c (exception :: EXCEPTION, http :: HTTP, ref :: REF | e) r ->
         Request ->
         Response ->
         Eff (exception :: EXCEPTION, http :: HTTP, ref :: REF | e) Unit
serve app req res =
  let readable = requestAsStream req

      onDataString' ref chunk =
        readRef ref >>=
          (writeRef ref) <<<
          Just <<<
          maybe chunk (flip append chunk)

      onError' =
        const $ responsify res 500 $ messageify "Failed reading requested body"

      onEnd' ref = do
        body <- readRef ref
        response <- runApp (Context { req, res, body }) app
        case response of
          Left (Failure f) -> responsify res f.status $ messageify f.message
          Right (Result r) -> responsify res r.status $ maybe "" encodeJSON r.body

   in do
     ref <- newRef Nothing
     onDataString readable UTF8 $ onDataString' ref
     onError readable $ onError'
     onEnd readable $ onEnd' ref




runApp :: forall c e r.
          Encode r =>
          Context ->
          App c e r ->
          Eff e (Either Failure (Result r))
runApp ctx (App app) = runExceptT do
  validateHttpMethod ctx
  handler <- runRouter ctx app.router
  c <- runBefore ctx app.before
  r <- runHandler c handler
  runAfter r app.after



validateHttpMethod :: forall e.
                      Context ->
                      ExceptT Failure (Eff e) Unit
validateHttpMethod (Context { req }) =
  case (requestMethod req) of
    "POST" -> pure unit
    _ -> throwError $ Failure
      { status: 405
      , message: "You can use POST only."
      }



runRouter :: forall c e r.
             Encode r =>
             Context ->
             Router c e r ->
             ExceptT Failure (Eff e) (Handler c e r)
runRouter (Context { req }) (Router r) =
  case (T.lookup (requestURL req) r) of
    Just handler -> pure handler
    Nothing -> throwError $ Failure
      { status: 404
      , message: "The path is unknown."
      }



runHandler :: forall c e r.
              Encode r =>
              c ->
              Handler c e r ->
              ExceptT Failure (Eff e) (Result r)
runHandler c (Handler f) = f c



runBefore :: forall e c.
             Context ->
             Before e c ->
             ExceptT Failure (Eff e) c
runBefore ctx (Before f) = f ctx



runAfter :: forall e r.
            Encode r =>
            Result r ->
            After e r ->
            ExceptT Failure (Eff e) (Result r)
runAfter r (After f) = f r



responsify :: forall e.
              Response ->
              Int ->
              String ->
              Eff (http :: HTTP | e) Unit
responsify res status body =
  let writable = responseAsStream res
   in do
     setHeader res "Content-Type" "application/json"
     setStatusCode res status
     void $ writeString writable UTF8 body $ pure unit
     end writable $ pure unit



messageify :: String -> String
messageify msg ="{ \"message\": \"" <> msg <> "\" }"



parseBody :: forall e a. Decode a => Context -> ExceptT Failure (Eff e) a
parseBody (Context { body: Nothing }) = throwError noBodyFailure
parseBody (Context { req, body: Just body' }) =
  case SM.lookup "content-type" (requestHeaders req) >>= parseMediaType of
    Just mediaType | mediaType == applicationJSON ->
      case (runExcept $ decodeJSON body') of
        Left _ -> throwError entityFailure
        Right b -> pure b
    _ -> throwError mediaTypeFailure



parseMediaType :: String -> Maybe MediaType
parseMediaType = split (Pattern ";") >>> head >>> map MediaType



noBodyFailure :: Failure
noBodyFailure =
  Failure
    { status: 400
    , message: "Need request body."
    }



entityFailure :: Failure
entityFailure =
  Failure
    { status: 422
    , message: "Received invalid body."
    }



mediaTypeFailure :: Failure
mediaTypeFailure =
  Failure
    { status: 400
    , message: "Received unpermitted Content-Type."
    }



handle :: forall c e r.
          Encode r =>
          String ->
          (c -> ExceptT Failure (Eff e) (Result r)) ->
          Tuple String (Handler c e r)
handle path proc = Tuple path $ Handler proc

infix 4 handle as :>



defaultBefore :: forall e. Before e Context
defaultBefore = Before $ \ctx -> pure ctx



defaultAfter :: forall e r. Encode r => After e r
defaultAfter = After $ \result -> pure result



defaultApp :: forall e r. Encode r => Router Context e r -> App Context e r
defaultApp router =
  App
    { before: defaultBefore
    , router
    , after: defaultAfter
    }
