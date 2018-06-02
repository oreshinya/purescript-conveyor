module Conveyor.Logger
  ( withLogger
  ) where

import Prelude

import Conveyor.Types (LogInfo(..), Logger(..), Responder(..), RawData)
import Data.JSDate (now, toString)
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Global.Unsafe (unsafeStringify)
import Node.Stdout (log)



withLogger :: forall server. server -> Logger server
withLogger = Logger simpleLog



simpleLog :: RawData -> LogInfo -> Aff Unit
simpleLog rawData logInfo = liftEffect do
  timeStr <- toString <$> now
  case logInfo of
    Start ->
      log $ "info:START\ttime:" <> timeStr <> "\tpath:" <> rawData.path <> "\tbody:" <> replace whiteSpaceRegex "" rawData.rawBody
    End (Responder r) ->
      log $ "info:END\ttime:" <> timeStr <> "\tpath:" <> rawData.path <> "\tbody:" <> unsafeStringify r.body



whiteSpaceRegex :: Regex
whiteSpaceRegex = unsafeRegex "\\s" global
