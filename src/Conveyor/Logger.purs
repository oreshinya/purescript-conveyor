module Conveyor.Logger
  ( LogInfo(..)
  , Logger(..)
  , withLogger
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Conveyor.Argument (RawData(..))
import Conveyor.Respondable (Responder(..))
import Data.DateTime.Locale (LocalValue(..))
import Data.JSDate (fromDateTime, toString)
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Global.Unsafe (unsafeStringify)
import Node.HTTP (requestURL)
import Node.Stdout (log)



data LogInfo
  = Start
  | End Responder

data Logger e s = Logger (RawData -> LogInfo -> Aff e Unit) s



withLogger :: forall e s. s -> Logger (console :: CONSOLE, now :: NOW | e) s
withLogger = Logger simpleLog



simpleLog :: forall e. RawData -> LogInfo -> Aff (console :: CONSOLE, now :: NOW | e) Unit
simpleLog (RawData rd) logInfo = liftEff do
  LocalValue _ dateTime <- nowDateTime
  let timeStr = toString $ fromDateTime dateTime
  let path = requestURL rd.req
  case logInfo of
    Start ->
      log $ "info:START\ttime:" <> timeStr <> "\tpath:" <> path <> "\tbody:" <> replace whiteSpaceRegex "" rd.rawBody
    End (Responder r) ->
      log $ "info:END\ttime:" <> timeStr <> "\tpath:" <> path <> "\tbody:" <> unsafeStringify r.body



whiteSpaceRegex :: Regex
whiteSpaceRegex = unsafeRegex "\\s" global
