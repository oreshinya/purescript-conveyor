module Conveyor.Handler
  ( Context(..)
  , Handler
  , askExtra
  , askRaw
  , runHandler
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Plus (class Plus)
import Conveyor.Types (RawData)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)



newtype Context ex =
  Context
    { extraData :: ex
    , rawData :: RawData
    }



newtype Handler ex a = Handler (ReaderT (Context ex) Aff a)

derive newtype instance functorAuthHander :: Functor (Handler ex)
derive newtype instance applyAuthHander :: Apply (Handler ex)
derive newtype instance applicativeAuthHander :: Applicative (Handler ex)
derive newtype instance altAuthHandler :: Alt (Handler ex)
derive newtype instance plusAuthHandler :: Plus (Handler ex)
derive newtype instance bindAuthHandler :: Bind (Handler ex)
derive newtype instance monadAuthHandler :: Monad (Handler ex)
derive newtype instance semigroupAuthHandler :: Semigroup a => Semigroup (Handler ex a)
derive newtype instance monoidAuthHandler :: Monoid a => Semigroup (Handler ex a)
derive newtype instance monadEffAuthHandler :: MonadEffect (Handler ex)
derive newtype instance monadAffAuthHandler :: MonadAff (Handler ex)
derive newtype instance monadThrowAuthHandler :: MonadThrow Error (Handler ex)
derive newtype instance monadErrorAuthHandler :: MonadError Error (Handler ex)
derive newtype instance monadAskAuthHandler :: MonadAsk (Context ex) (Handler ex)
derive newtype instance monadRecAuthHandler :: MonadRec (Handler ex)



askExtra :: forall ex. Handler ex ex
askExtra = flip map ask \(Context ctx) -> ctx.extraData



askRaw :: forall ex. Handler ex RawData
askRaw = flip map ask \(Context ctx) -> ctx.rawData



runHandler :: forall ex a. Handler ex a -> Context ex -> Aff a
runHandler (Handler readerT) = runReaderT readerT
