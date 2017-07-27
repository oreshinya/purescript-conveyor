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
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(ForeignError), F)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (decodeJSON)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (unwrap)
import Data.StrMap (lookup)
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (HTTP, Request, Response, requestHeaders, requestAsStream)
import Node.Stream (onDataString, onError, onEnd)
import Type.Row (class RowToList, kind RowList, Cons, Nil)
import Unsafe.Coerce (unsafeCoerce)

import Conveyor.Handler (Handler)
import Conveyor.Internal (LProxy(..), get, respond, rowToList)
import Conveyor.Responsable (class Responsable, errorMsg)



class Servable e s | s -> e where
  serve :: s -> Request -> Response -> String -> Maybe (Eff (http :: HTTP | e) Unit)



class ServableList e (l :: RowList) (r :: # Type) | l -> r where
  serveList
    :: LProxy l
    -> Record r
    -> Request
    -> Response
    -> String
    -> Maybe (Eff (http :: HTTP | e) Unit)



decodeBody :: forall a. Decode a => Request -> String -> F a
decodeBody req body =
  case lookup "content-type" (requestHeaders req) >>= parseMediaType of
    Just mediaType | mediaType == applicationJSON -> decodeJSON body
    _ -> throwError $ singleton $ ForeignError "Received unpermitted Content-Type."



parseMediaType :: String -> Maybe MediaType
parseMediaType = split (Pattern ";") >>> head >>> map MediaType



instance servableHandler :: Responsable r => Servable e (Handler e r) where
  serve handler req res _ =
    let onError' = const $ respond res $ errorMsg 500 "Internal server error"
        onSuccess' r = respond res r
     in pure $ void $ runAff onError' onSuccess' $ unwrap handler



instance servableWithBody :: (Decode b, Servable e s) => Servable e (b -> s) where
  serve handler req res path =
    let readable = requestAsStream req

        onDataString' ref chunk =
          readRef ref >>= writeRef ref <<< flip append chunk

        onError' = const $ respond res $ errorMsg 500 "Failed reading requested body"

        onEnd' ref = do
          body <- readRef ref
          case runExcept $ decodeBody req body of
            Left _ -> respond res $ errorMsg 400 "Request body is invalid"
            Right b ->
              case serve (handler b) req res path of
                Nothing -> respond res $ errorMsg 404 "No such route"
                Just e -> unsafeCoerceEff e

     in Just $ unsafeCoerceEff do
       ref <- newRef ""
       onDataString readable UTF8 $ onDataString' ref
       onError readable $ onError'
       onEnd readable $ onEnd' ref



instance servableRecord :: (RowToList r l, ServableList e l r) => Servable e (Record r) where
  serve r = serveList (rowToList r) r



instance servableListNil :: ServableList e Nil () where
  serveList _ _ _ _ _ = Nothing



instance servableListCons :: (IsSymbol route, Servable e s, ServableList e l r1, RowCons route s r1 r) => ServableList e (Cons route s l) r where
  serveList _ rec req res path
    | path == reflectSymbol (SProxy :: SProxy route) = serve (get (SProxy :: SProxy route) rec :: s) req res path
    | otherwise = serveList (LProxy :: LProxy l) (unsafeCoerce rec) req res path
