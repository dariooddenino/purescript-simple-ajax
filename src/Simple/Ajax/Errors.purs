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
  , _formatError
  , _serverError
  , mapBasicError
  , parseError
  , statusOk
  ) where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Data.Variant (Variant, inj)
import Foreign (ForeignError, MultipleErrors)
import Type.Prelude (SProxy(..))

-- | Basic error type containing the common http errors.
type HTTPError = BasicError ()
-- | Extends `HTTPError` to add json parsing errors.
type AjaxError = BasicError ParseError

statusOk :: StatusCode -> Boolean
statusOk (StatusCode n) = n >= 200 && n < 300

_parseError = SProxy :: SProxy "parseError"
_badRequest = SProxy :: SProxy "badRequest"
_unAuthorized = SProxy :: SProxy "unAuthorized"
_forbidden = SProxy :: SProxy "forbidden"
_notFound = SProxy :: SProxy "notFound"
_methodNotAllowed = SProxy :: SProxy "methodNotAllowed"
_formatError = SProxy :: SProxy "formatError"
_serverError = SProxy :: SProxy "serverError"

type ParseError = (parseError :: MultipleErrors)
type BasicErrorRow e =
    ( badRequest :: String
    , unAuthorized :: Unit
    , forbidden :: Unit
    , notFound :: Unit
    , methodNotAllowed :: Unit
    , formatError :: ForeignError
    , serverError :: String
    | e
    )
type BasicError e = Variant (BasicErrorRow e)

mapBasicError :: StatusCode -> String -> BasicError ()
mapBasicError (StatusCode n) m
  | n == 400 = inj _badRequest m
  | n == 401 = inj _unAuthorized unit
  | n == 403 = inj _forbidden unit
  | n == 404 = inj _notFound unit
  | n == 405 = inj _methodNotAllowed unit
  | otherwise = inj _serverError m

parseError :: MultipleErrors -> Variant ParseError
parseError = inj _parseError
