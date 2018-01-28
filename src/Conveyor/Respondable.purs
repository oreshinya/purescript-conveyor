module Conveyor.Respondable where


import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Foreign (Foreign, toForeign)
import Global.Unsafe (unsafeStringify)
import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP, Response, responseAsStream, setHeader, setStatusCode)
import Node.Stream (end, writeString)



newtype Responder = Responder
  { contentType :: String
  , code :: Int
  , body :: Foreign
  }



class Respondable r where
  toResponder :: r -> Responder
  fromError :: Error -> r



conveyorError :: Int -> String -> Responder
conveyorError code message =
  Responder
    { contentType: "application/json; charset=utf-8"
    , code
    , body: toForeign { message }
    }



send
  :: forall e
   . Response
  -> Responder
  -> Eff (http :: HTTP | e) Unit
send res (Responder r) = do
  setHeader res "Connection" "close"
  setHeader res "Content-Type" r.contentType
  setStatusCode res r.code
  void $ writeString writable UTF8 (unsafeStringify r.body) $ pure unit
  end writable $ pure unit
  where
    writable = responseAsStream res
