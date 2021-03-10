module Simple.Ajax.Errors
  ( BasicError
  , BasicErrorRow
  , ParseError
  , HTTPError
  , AjaxError
  , _parseError
  , _badRequest
  , _unAuthorized
  , _forbidden
  , _notFound
  , _methodNotAllowed
  , _serverError
  , _affjaxError
  , mapBasicError
  , parseError
  , statusOk
  ) where

import Prelude

import Affjax as AX
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Maybe (Maybe, fromMaybe)
import Data.Variant (Variant, inj)
import Type.Prelude (Proxy(..))

-- | Basic error type containing the common http errors.
type HTTPError = BasicError ()
-- | Extends `HTTPError` to add json parsing errors.
type AjaxError = BasicError ParseError

statusOk :: StatusCode -> Boolean
statusOk (StatusCode n) = n >= 200 && n < 300

_parseError = Proxy :: Proxy "parseError"
_badRequest = Proxy :: Proxy "badRequest"
_unAuthorized = Proxy :: Proxy "unAuthorized"
_forbidden = Proxy :: Proxy "forbidden"
_notFound = Proxy :: Proxy "notFound"
_methodNotAllowed = Proxy :: Proxy "methodNotAllowed"
_serverError = Proxy :: Proxy "serverError"
_affjaxError = Proxy :: Proxy "affjaxError"

-- TODO formatError?
type ParseError = (parseError :: JsonDecodeError)
type BasicErrorRow e =
    ( badRequest :: String
    , unAuthorized :: Unit
    , forbidden :: Unit
    , notFound :: Unit
    , methodNotAllowed :: Unit
    , serverError :: String
    , affjaxError :: AX.Error
    | e
    )
type BasicError e = Variant (BasicErrorRow e)

-- TODO how to handle this new Maybe thing?
mapBasicError :: StatusCode -> Maybe String -> BasicError ()
mapBasicError (StatusCode n) m
  | n == 400 = inj _badRequest $ fromMaybe "" m
  | n == 401 = inj _unAuthorized unit
  | n == 403 = inj _forbidden unit
  | n == 404 = inj _notFound unit
  | n == 405 = inj _methodNotAllowed unit
  | otherwise = inj _serverError $ fromMaybe "" m

parseError :: JsonDecodeError -> Variant ParseError
parseError = inj _parseError
