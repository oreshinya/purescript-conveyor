module Conveyor.Handler (Handler) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Data.Newtype (class Newtype)
import Node.HTTP (HTTP)


newtype Handler e r = Handler (Aff (http :: HTTP | e) r)

derive instance newtypeHandler :: Newtype (Handler e r) _

derive newtype instance functorHandler :: Functor (Handler e)
derive newtype instance applyHanlder :: Apply (Handler e)
derive newtype instance applicativeHandler :: Applicative (Handler e)
derive newtype instance bindHandler :: Bind (Handler e)
derive newtype instance monadHanlder :: Monad (Handler e)
derive newtype instance monadEffHandler :: MonadEff (http :: HTTP | e) (Handler e)
derive newtype instance monadAffHanlder :: MonadAff (http :: HTTP | e) (Handler e)
derive newtype instance monadErrorHandler :: MonadError Error (Handler e)
derive newtype instance monadThrowHandler :: MonadThrow Error (Handler e)
