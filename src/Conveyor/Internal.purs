module Conveyor.Internal
  ( LProxy(..)
  , get
  , rowToList
  ) where

import Prelude
import Data.Maybe (fromMaybe')
import Data.StrMap (lookup)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Partial.Unsafe (unsafeCrashWith)
import Type.Row (class RowToList, kind RowList)
import Unsafe.Coerce (unsafeCoerce)



data LProxy (l :: RowList) = LProxy



get
  :: forall l a r1 r2
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
