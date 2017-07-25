module Conveyor.Internal
  ( LProxy(..)
  , get
  , respond
  , rowToList
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (fromMaybe')
import Data.StrMap (lookup)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (HTTP, Response, responseAsStream, setHeader, setStatusCode)
import Node.Stream (end, writeString)
import Partial.Unsafe (unsafeCrashWith)
import Type.Row (class RowToList, kind RowList)
import Unsafe.Coerce (unsafeCoerce)
import Conveyor.Responsable (class Responsable, statusCode, encodeBody)


data LProxy (l :: RowList) = LProxy



get :: forall l a r1 r2
     . IsSymbol l
    => RowCons l a r1 r2
    => SProxy l
    -> Record r2
    -> a
get l r =
    fromMaybe'
      (\_ -> unsafeCrashWith ("unsafeGet: missing key " <> show s))
      (lookup s (unsafeCoerce r))
  where
    s = reflectSymbol l



rowToList :: forall proxy r l. RowToList r l => proxy r -> LProxy l
rowToList _ = LProxy



respond :: forall e r.
           Responsable r =>
           Response ->
           r ->
           Eff (http :: HTTP | e) Unit
respond res r =
  let writable = responseAsStream res
   in do
     setHeader res "Content-Type" "application/json"
     setStatusCode res $ statusCode r
     void $ writeString writable UTF8 (encodeBody r) $ pure unit
     end writable $ pure unit
