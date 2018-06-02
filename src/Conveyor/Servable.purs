module Conveyor.Servable where

import Prelude

import Conveyor.Handler (Context(..), Handler, runHandler)
import Conveyor.Internal (BatchParams, LProxy(..), batchResponder, conveyorError, decodeBody, get, logError, rowToList)
import Conveyor.Respondable (class RespondableError, fromError, toResponder)
import Conveyor.Types (Batch(..), Body(..), LogInfo(..), Logger(..), Responder, RawData)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Foreign (MultipleErrors)
import Node.HTTP (requestMethod)
import Prim.Row (class Cons)
import Simple.JSON (class ReadForeign, writeJSON)
import Type.Row (class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)



class Servable ex server where
  serve :: server -> ex -> RawData -> Aff Responder



class ServableList ex (l :: RowList) (r :: # Type) | l -> r where
  serveList :: LProxy l -> Record r -> ex -> RawData -> Aff Responder



instance servableAff :: (RespondableError r) => Servable ex (Aff r) where
  serve aff _ rawData =
    case requestMethod rawData.req of
      "POST" -> do
         result <- attempt aff
         case result of
           Left err -> do
             liftEffect $ logError err
             pure $ toResponder $ (fromError err :: r)
           Right suc ->
             pure $ toResponder suc
      _ -> pure $ conveyorError 400 "HTTP Method is not POST"



instance servableHandler :: (RespondableError r) => Servable ex (Handler ex r) where
  serve handler extraData rawData =
    serve (runHandler handler $ Context { extraData, rawData }) extraData rawData



instance servableWithBody :: (ReadForeign b, Servable ex server) => Servable ex (Body b -> server) where
  serve server extraData rawData =
    case decodeBody rawData.req rawData.rawBody of
      Left _ -> pure $ conveyorError 400 "Request body is invalid"
      Right body -> serve (server $ Body body) extraData rawData



instance servableBatch :: Servable ex server => Servable ex (Batch server) where
  serve (Batch server) extraData rawData =
    let isBatch = rawData.path == "batch" && requestMethod rawData.req == "POST"

        decoded :: Either MultipleErrors BatchParams
        decoded = decodeBody rawData.req rawData.rawBody

        onIterate { path, body } =
          serve server extraData
            rawData
              { path = path
              , rawBody = writeJSON body
              }

     in if isBatch then
          case decoded of
            Left _ -> pure $ conveyorError 400 "Invalid batch request"
            Right bodies -> map batchResponder $ traverse onIterate bodies
        else serve server extraData rawData



instance servableLogger :: Servable ex server => Servable ex (Logger server) where
  serve (Logger logger server) extraData rawData = do
    logger rawData Start
    responder <- serve server extraData rawData
    logger rawData $ End responder
    pure responder



instance servableRecord :: (RowToList r l, ServableList ex l r) => Servable ex (Record r) where
  serve rec = serveList (rowToList rec) rec



instance servableListNil :: ServableList ex Nil () where
  serveList _ _ _ _ = pure $ conveyorError 404 "No such route"



instance servableListCons :: (IsSymbol route, Servable ex server, ServableList ex l r1, Cons route server r1 r) => ServableList ex (Cons route server l) r where
  serveList _ rec extraData rawData
    | rawData.path == reflectSymbol (SProxy :: SProxy route) = serve (get (SProxy :: SProxy route) rec :: server) extraData rawData
    | otherwise = serveList (LProxy :: LProxy l) (unsafeCoerce rec) extraData rawData
