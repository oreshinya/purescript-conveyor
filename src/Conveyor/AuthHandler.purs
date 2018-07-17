module Conveyor.AuthHandler
  ( AuthContenxt(..)
  , AuthHandler
  , askExtra
  , askRaw
  , askAuthTarget
  , runAuthHandler
  , class Authenticatable, authenticate
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



newtype AuthContenxt a ex =
  AuthContenxt
    { authTarget :: a
    , extraData :: ex
    , rawData :: RawData
    }



newtype AuthHandler a ex b = AuthHandler (ReaderT (AuthContenxt a ex) Aff b)

derive newtype instance functorAuthHander :: Functor (AuthHandler a ex)
derive newtype instance applyAuthHander :: Apply (AuthHandler a ex)
derive newtype instance applicativeAuthHander :: Applicative (AuthHandler a ex)
derive newtype instance altAuthHandler :: Alt (AuthHandler a ex)
derive newtype instance plusAuthHandler :: Plus (AuthHandler a ex)
derive newtype instance bindAuthHandler :: Bind (AuthHandler a ex)
derive newtype instance monadAuthHandler :: Monad (AuthHandler a ex)
derive newtype instance semigroupAuthHandler :: Semigroup b => Semigroup (AuthHandler a ex b)
derive newtype instance monoidAuthHandler :: Monoid b => Semigroup (AuthHandler a ex b)
derive newtype instance monadEffAuthHandler :: MonadEffect (AuthHandler a ex)
derive newtype instance monadAffAuthHandler :: MonadAff (AuthHandler a ex)
derive newtype instance monadThrowAuthHandler :: MonadThrow Error (AuthHandler a ex)
derive newtype instance monadErrorAuthHandler :: MonadError Error (AuthHandler a ex)
derive newtype instance monadAskAuthHandler :: MonadAsk (AuthContenxt a ex) (AuthHandler a ex)
derive newtype instance monadRecAuthHandler :: MonadRec (AuthHandler a ex)



askExtra :: forall a ex. AuthHandler a ex ex
askExtra = flip map ask \(AuthContenxt ctx) -> ctx.extraData



askRaw :: forall a ex. AuthHandler a ex RawData
askRaw = flip map ask \(AuthContenxt ctx) -> ctx.rawData



askAuthTarget :: forall a ex. AuthHandler a ex a
askAuthTarget = flip map ask \(AuthContenxt ctx) -> ctx.authTarget



runAuthHandler :: forall a ex b. AuthHandler a ex b -> AuthContenxt a ex -> Aff b
runAuthHandler (AuthHandler readerT) = runReaderT readerT


class Authenticatable ex a where
  authenticate :: ex -> RawData -> Aff a
