module Conveyor.Servable where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Class (liftEff)
import Conveyor.Handler (Context(..), Handler, runHandler)
import Conveyor.Internal (BatchParams, LProxy(..), batchResponder, conveyorError, decodeBody, get, logError, rowToList)
import Conveyor.Respondable (class RespondableError, fromError, toResponder)
import Conveyor.Types (Batch(..), Body(..), LogInfo(..), Logger(..), Responder, RawData)
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Node.HTTP (requestMethod)
import Simple.JSON (class ReadForeign, writeJSON)
import Type.Row (class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)



class Servable ex eff server | server -> eff where
  serve :: server -> ex -> RawData -> Aff eff Responder



class ServableList ex eff (l :: RowList) (r :: # Type) | l -> r where
  serveList :: LProxy l -> Record r -> ex -> RawData -> Aff eff Responder



instance servableAff :: (RespondableError r) => Servable ex eff (Aff eff r) where
  serve aff _ rawData =
    case requestMethod rawData.req of
      "POST" -> do
         result <- attempt aff
         case result of
           Left err -> do
             liftEff $ logError err
             pure $ toResponder $ (fromError err :: r)
           Right suc ->
             pure $ toResponder suc
      _ -> pure $ conveyorError 400 "HTTP Method is not POST"



instance servableHandler :: (RespondableError r) => Servable ex eff (Handler ex eff r) where
  serve handler extraData rawData =
    serve (runHandler handler $ Context { extraData, rawData }) extraData rawData



instance servableWithBody :: (ReadForeign b, Servable ex eff server) => Servable ex eff (Body b -> server) where
  serve server extraData rawData =
    case decodeBody rawData.req rawData.rawBody of
      Left _ -> pure $ conveyorError 400 "Request body is invalid"
      Right body -> serve (server $ Body body) extraData rawData



instance servableBatch :: Servable ex eff server => Servable ex eff (Batch server) where
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



instance servableLogger :: Servable ex eff server => Servable ex eff (Logger eff server) where
  serve (Logger logger server) extraData rawData = do
    logger rawData Start
    responder <- serve server extraData rawData
    logger rawData $ End responder
    pure responder



instance servableRecord :: (RowToList r l, ServableList ex eff l r) => Servable ex eff (Record r) where
  serve rec = serveList (rowToList rec) rec



instance servableListNil :: ServableList ex eff Nil () where
  serveList _ _ _ _ = pure $ conveyorError 404 "No such route"



instance servableListCons :: (IsSymbol route, Servable ex eff server, ServableList ex eff l r1, RowCons route server r1 r) => ServableList ex eff (Cons route server l) r where
  serveList _ rec extraData rawData
    | rawData.path == reflectSymbol (SProxy :: SProxy route) = serve (get (SProxy :: SProxy route) rec :: server) extraData rawData
    | otherwise = serveList (LProxy :: LProxy l) (unsafeCoerce rec) extraData rawData
