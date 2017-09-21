module Conveyor.Servable
  ( class Servable, serve
  , class ServableList, serveList
  ) where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept, throwError)
import Conveyor.Body (Body(..))
import Conveyor.Context (Context(..))
import Conveyor.Handler (Handler)
import Conveyor.Internal (LProxy(..), get, rowToList)
import Conveyor.Readable (class Readable, readBody)
import Conveyor.Respondable (class Respondable, ConveyorError(..), respond, systemError)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(ForeignError), F)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (unwrap)
import Data.StrMap (lookup)
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (HTTP, Request, Response, requestAsStream, requestHeaders, requestMethod)
import Node.Stream (onDataString, onError, onEnd)
import Type.Row (class RowToList, kind RowList, Cons, Nil)
import Unsafe.Coerce (unsafeCoerce)



class Servable c e s | s -> e where
  serve :: c -> s -> Request -> Response -> String -> Maybe (Eff (http :: HTTP | e) Unit)



class ServableList c e (l :: RowList) (r :: # Type) | l -> r where
  serveList :: c -> LProxy l -> Record r -> Request -> Response -> String -> Maybe (Eff (http :: HTTP | e) Unit)



instance servableHandler :: Respondable r => Servable c e (Handler e r) where
  serve _ handler req res _ =
    let method = requestMethod req
        onError' err = respond res $ (systemError err :: r)
        onSuccess' r = respond res r
     in case method of
          "POST" -> pure $ void $ runAff onError' onSuccess' $ unwrap handler
          _ -> pure $ respond res $ ConveyorError 400 "HTTP Method is not POST"



instance servableWithBody :: (Readable b, Servable c e s) => Servable c e (Body b -> s) where
  serve ctx handler req res path =
    let readable = requestAsStream req

        onDataString' ref chunk =
          readRef ref >>= writeRef ref <<< flip append chunk

        onError' = const $ respond res $ ConveyorError 500 "Failed reading requested body"

        onEnd' ref = do
          body <- readRef ref
          case runExcept $ decodeBody req body of
            Left _ -> respond res $ ConveyorError 400 "Request body is invalid"
            Right b ->
              case serve ctx (handler $ Body b) req res path of
                Nothing -> respond res $ ConveyorError 404 "No such route"
                Just e -> unsafeCoerceEff e

     in Just $ unsafeCoerceEff do
       ref <- newRef ""
       onDataString readable UTF8 $ onDataString' ref
       onError readable $ onError'
       onEnd readable $ onEnd' ref



instance servableWithContext :: Servable c e s => Servable c e (Context c -> s) where
  serve ctx handler req res path = serve ctx (handler $ Context ctx) req res path



instance servableRecord :: (RowToList r l, ServableList c e l r) => Servable c e (Record r) where
  serve ctx handler = serveList ctx (rowToList handler) handler



instance servableListNil :: ServableList c e Nil () where
  serveList _ _ _ _ _ _ = Nothing



instance servableListCons :: (IsSymbol route, Servable c e s, ServableList c e l r1, RowCons route s r1 r) => ServableList c e (Cons route s l) r where
  serveList ctx _ rec req res path
    | path == reflectSymbol (SProxy :: SProxy route) = serve ctx (get (SProxy :: SProxy route) rec :: s) req res path
    | otherwise = serveList ctx (LProxy :: LProxy l) (unsafeCoerce rec) req res path



decodeBody :: forall a. Readable a => Request -> String -> F a
decodeBody req body =
  case lookup "content-type" (requestHeaders req) >>= parseMediaType of
    Just mediaType | mediaType == applicationJSON -> readBody body
    _ -> throwError $ singleton $ ForeignError "Received unpermitted Content-Type."



parseMediaType :: String -> Maybe MediaType
parseMediaType = split (Pattern ";") >>> head >>> map MediaType
