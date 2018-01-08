module Conveyor.Servable where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Conveyor.Argument (Body(..), Context(..), RawData(..))
import Conveyor.Batch (Batch(..), BatchParams, batchResponder)
import Conveyor.Internal (LProxy(..), decodeBody, get, rowToList)
import Conveyor.Respondable (class Respondable, Responder, conveyorError, fromError, toResponder)
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Node.HTTP (requestMethod)
import Simple.JSON (class ReadForeign)
import Type.Row (class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)



class Servable c e s | s -> e where
  serve :: s -> c -> RawData -> Aff e Responder



class ServableList c e (l :: RowList) (r :: # Type) | l -> r where
  serveList :: LProxy l -> Record r -> c -> RawData -> Aff e Responder



instance servableAff :: Respondable r => Servable c e (Aff e r) where
  serve aff _ (RawData rd) =
    case requestMethod rd.req of
      "POST" -> do
         result <- attempt aff
         pure <<< toResponder $ case result of
           Left err -> fromError err
           Right suc -> suc
      _ -> pure $ conveyorError 400 "HTTP Method is not POST"



instance servableWithBody :: (ReadForeign b, Servable c e s) => Servable c e (Body b -> s) where
  serve servable ctx rawData@(RawData rd) =
    case decodeBody rd.req rd.rawBody of
      Left _ -> pure $ conveyorError 400 "Request body is invalid"
      Right body -> serve (servable $ Body body) ctx rawData



instance servableWithContext :: Servable c e s => Servable c e (Context c -> s) where
  serve servable ctx = serve (servable $ Context ctx) ctx



instance servableWithRawData :: Servable c e s => Servable c e (RawData -> s) where
  serve servable ctx rawData = serve (servable rawData) ctx rawData



instance servableBatch :: Servable c e s => Servable c e (Batch s) where
  serve (Batch servable) ctx rawData@(RawData rd) =
    let isBatch = rd.path == "batch" && requestMethod rd.req == "POST"

        decoded :: Either MultipleErrors BatchParams
        decoded = decodeBody rd.req rd.rawBody

        onIterate { path, rawBody } =
          serve servable ctx $
            RawData
              { req: rd.req
              , res: rd.res
              , path
              , rawBody
              }

     in if isBatch then
          case decoded of
            Left _ -> pure $ conveyorError 400 "Batch request body is invalid"
            Right bodies -> map batchResponder $ traverse onIterate bodies
        else serve servable ctx rawData



instance servableRecord :: (RowToList r l, ServableList c e l r) => Servable c e (Record r) where
  serve rec = serveList (rowToList rec) rec



instance servableListNil :: ServableList c e Nil () where
  serveList _ _ _ _ = pure $ conveyorError 404 "No such route"



instance servableListCons :: (IsSymbol route, Servable c e s, ServableList c e l r1, RowCons route s r1 r) => ServableList c e (Cons route s l) r where
  serveList _ rec ctx rawData@(RawData rd)
    | rd.path == reflectSymbol (SProxy :: SProxy route) = serve (get (SProxy :: SProxy route) rec :: s) ctx rawData
    | otherwise = serveList (LProxy :: LProxy l) (unsafeCoerce rec) ctx rawData
