module Conveyor.Handler
  ( Context(..)
  , Handler
  , askExtra
  , askRaw
  , runHandler
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Plus (class Plus)
import Conveyor.Types (RawData)
import Data.Monoid (class Monoid)



newtype Context ex =
  Context
    { extraData :: ex
    , rawData :: RawData
    }



newtype Handler ex eff a = Handler (ReaderT (Context ex) (Aff eff) a)

derive newtype instance functorAuthHander :: Functor (Handler ex eff)
derive newtype instance applyAuthHander :: Apply (Handler ex eff)
derive newtype instance applicativeAuthHander :: Applicative (Handler ex eff)
derive newtype instance altAuthHandler :: Alt (Handler ex eff)
derive newtype instance plusAuthHandler :: Plus (Handler ex eff)
derive newtype instance bindAuthHandler :: Bind (Handler ex eff)
derive newtype instance monadAuthHandler :: Monad (Handler ex eff)
derive newtype instance semigroupAuthHandler :: Semigroup a => Semigroup (Handler ex eff a)
derive newtype instance monoidAuthHandler :: Monoid a => Semigroup (Handler ex eff a)
derive newtype instance monadEffAuthHandler :: MonadEff eff (Handler ex eff)
derive newtype instance monadAffAuthHandler :: MonadAff eff (Handler ex eff)
derive newtype instance monadThrowAuthHandler :: MonadThrow Error (Handler ex eff)
derive newtype instance monadErrorAuthHandler :: MonadError Error (Handler ex eff)
derive newtype instance monadAskAuthHandler :: MonadAsk (Context ex) (Handler ex eff)
derive newtype instance monadRecAuthHandler :: MonadRec (Handler ex eff)



askExtra :: forall ex eff. Handler ex eff ex
askExtra = flip map ask \(Context ctx) -> ctx.extraData



askRaw :: forall ex eff. Handler ex eff RawData
askRaw = flip map ask \(Context ctx) -> ctx.rawData



runHandler :: forall ex eff a. Handler ex eff a -> Context ex -> Aff eff a
runHandler (Handler readerT) = runReaderT readerT
