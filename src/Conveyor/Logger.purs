module Conveyor.Logger
  ( withLogger
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Conveyor.Types (LogInfo(..), Logger(..), Responder(..), RawData)
import Data.DateTime.Locale (LocalValue(..))
import Data.JSDate (fromDateTime, toString)
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Global.Unsafe (unsafeStringify)
import Node.Stdout (log)



withLogger :: forall eff server. server -> Logger (console :: CONSOLE, now :: NOW | eff) server
withLogger = Logger simpleLog



simpleLog :: forall eff. RawData -> LogInfo -> Aff (console :: CONSOLE, now :: NOW | eff) Unit
simpleLog rawData logInfo = liftEff do
  LocalValue _ dateTime <- nowDateTime
  let timeStr = toString $ fromDateTime dateTime
  case logInfo of
    Start ->
      log $ "info:START\ttime:" <> timeStr <> "\tpath:" <> rawData.path <> "\tbody:" <> replace whiteSpaceRegex "" rawData.rawBody
    End (Responder r) ->
      log $ "info:END\ttime:" <> timeStr <> "\tpath:" <> rawData.path <> "\tbody:" <> unsafeStringify r.body



whiteSpaceRegex :: Regex
whiteSpaceRegex = unsafeRegex "\\s" global
