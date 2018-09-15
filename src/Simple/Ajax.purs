module Simple.Ajax
  ( simpleRequest, simpleRequest_
  , postR, post, postR_, post_
  , putR, put, putR_, put_
  , deleteR, delete, deleteR_, delete_
  , patchR, patch, patchR_, patch_
  , module Simple.Ajax.Errors
  ) where

import Prelude

import Affjax (Response, URL, Request, defaultRequest, request)
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Variant (expand, inj)
import Effect.Aff (Aff, Error, try)
import Foreign (Foreign)
import Simple.Ajax.Errors (BasicError', ParseError, _formatError, _serverError, mapBasicError, parseError, statusOk)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

handleResponse ::
  forall b.
  ReadForeign b =>
  Either Error (Response (Either ResponseFormat.ResponseFormatError String)) ->
  Either (BasicError' ParseError) b
handleResponse res = case res of
  Left e -> Left $ inj _serverError $ show e
  Right response ->
    case response.body of
      Left (ResponseFormat.ResponseFormatError err _) -> Left $ inj _formatError err
      Right j
        | statusOk response.status -> lmap (expand <<< parseError) (readJSON j)
        | otherwise -> Left $ expand $ mapBasicError response.status j

handleResponse_ ::
  Either Error (Response (Either ResponseFormat.ResponseFormatError String)) ->
  Either (BasicError' ()) Unit
handleResponse_ res = case res of
  Left e -> Left $ inj _serverError $ show e
  Right response -> case response.body of
      Left (ResponseFormat.ResponseFormatError err _) -> Left $ inj _formatError err
      Right j
        | statusOk response.status -> Right unit
        | otherwise -> Left $ expand $ mapBasicError response.status j

defaultSimpleRequest :: Request String
defaultSimpleRequest = defaultRequest { responseFormat = ResponseFormat.string
                                      , headers = [ Accept (MediaType "application/json") ]
                                      }

-- | Makes an HTTP request and tries to parse the response json.
-- |
-- | Helper methods are provided for the most common requests.
simpleRequest :: forall a b.
  WriteForeign a =>
  ReadForeign b =>
  Either Method CustomMethod ->
  Request String ->
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ParseError) b)
simpleRequest method req url content = do
  res <- try $ request $ req { method = method
                             , url = url
                             , content = RequestBody.string <<< writeJSON <$> content
                             , responseFormat = ResponseFormat.string
                             }
  pure $ handleResponse res

-- | Makes an HTTP request ignoring the response payload.
-- |
-- | Helper methods are provided for the most common requests.
simpleRequest_ ::
  forall a.
  WriteForeign a =>
  Either Method CustomMethod ->
  Request String ->
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ()) Unit)
simpleRequest_ method req url content = do
  res <- try $ request $ req { method = method
                             , url = url
                             , content = RequestBody.string <<< writeJSON <$> content
                             }
  pure $ handleResponse_ res

-- | Makes a `GET` request, taking a `Request String` and an `URL` as arguments
-- | and then tries to parse the response json.
getR ::
  forall b.
  ReadForeign b =>
  Request String ->
  URL ->
  Aff (Either (BasicError' ParseError) b)
getR req url = do
  res <- try $ request $ req { method = Left GET
                             , url = url
                             , responseFormat = ResponseFormat.string
                             }
  pure $ handleResponse res

-- | Makes a `GET` request, taking an `URL` as argument
-- | and then tries to parse the response json.
get ::
  forall b.
  ReadForeign b =>
  URL ->
  Aff (Either (BasicError' ParseError) b)
get = getR defaultSimpleRequest

-- | Makes a `POST` request, taking a `Request String`, an `URL` and an optional payload
-- | and then tries to parse the response json.
postR :: forall a b.
  WriteForeign a =>
  ReadForeign b =>
  Request String ->
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ParseError) b)
postR = simpleRequest (Left POST)

-- | Makes a `POST` request, taking an `URL` and an optional payload
-- | trying to parse the response json.
post ::
  forall a b.
  WriteForeign a =>
  ReadForeign b =>
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ParseError) b)
post = postR defaultSimpleRequest

-- | Makes a `POST` request, taking a `Request String`, an `URL` and an optional payload,
-- | ignoring the response payload.
postR_ ::
  forall a.
  WriteForeign a =>
  Request String ->
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ()) Unit)
postR_ = simpleRequest_ (Left POST)

-- | Makes a `POST` request, taking an `URL` and an optional payload,
-- | ignoring the response payload.
post_ ::
  forall a.
  WriteForeign a =>
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ()) Unit)
post_ = postR_ defaultSimpleRequest

-- | Makes a `PUT` request, taking a `Request String`, an `URL` and an optional payload
-- | and then tries to parse the response json.
putR :: forall a b.
  WriteForeign a =>
  ReadForeign b =>
  Request String ->
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ParseError) b)
putR = simpleRequest (Left PUT)

-- | Makes a `PUT` request, taking an `URL` and an optional payload
-- | trying to parse the response json.
put ::
  forall a b.
  WriteForeign a =>
  ReadForeign b =>
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ParseError) b)
put = putR defaultSimpleRequest

-- | Makes a `PUT` request, taking a `Request String`, an `URL` and an optional payload,
-- | ignoring the response payload.
putR_ ::
  forall a.
  WriteForeign a =>
  Request String ->
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ()) Unit)
putR_ = simpleRequest_ (Left PUT)

-- | Makes a `PUT` request, taking an `URL` and an optional payload,
-- | ignoring the response payload.
put_ ::
  forall a.
  WriteForeign a =>
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ()) Unit)
put_ = putR_ defaultSimpleRequest

-- | Makes a `PATCH` request, taking a `Request String`, an `URL` and an optional payload
-- | and then tries to parse the response json.
patchR :: forall a b.
  WriteForeign a =>
  ReadForeign b =>
  Request String ->
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ParseError) b)
patchR = simpleRequest (Left PATCH)

-- | Makes a `PATCH` request, taking an `URL` and an optional payload
-- | trying to parse the response json.
patch ::
  forall a b.
  WriteForeign a =>
  ReadForeign b =>
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ParseError) b)
patch = patchR defaultSimpleRequest

-- | Makes a `PATCH` request, taking a `Request String`, an `URL` and an optional payload,
-- | ignoring the response payload.
patchR_ ::
  forall a.
  WriteForeign a =>
  Request String ->
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ()) Unit)
patchR_ = simpleRequest_ (Left PATCH)

-- | Makes a `PATCH` request, taking an `URL` and an optional payload,
-- | ignoring the response payload.
patch_ ::
  forall a.
  WriteForeign a =>
  URL ->
  Maybe a ->
  Aff (Either (BasicError' ()) Unit)
patch_ = patchR_ defaultSimpleRequest

-- | Makes a `DELETE` request, taking a `Request String` and an `URL`
-- | and then tries to parse the response json.
deleteR :: forall b.
  ReadForeign b =>
  Request String ->
  URL ->
  Aff (Either (BasicError' ParseError) b)
deleteR req url = simpleRequest (Left DELETE) req url (Nothing :: Maybe Foreign)

-- | Makes a `DELETE` request, taking an `URL`
-- | and then tries to parse the response json.
delete ::
  forall b.
  ReadForeign b =>
  URL ->
  Aff (Either (BasicError' ParseError) b)
delete = deleteR defaultSimpleRequest

-- | Makes a `DELETE` request, taking a `Request String` and an `URL`,
-- | ignoring the response payload.
deleteR_ ::
  Request String ->
  URL ->
  Aff (Either (BasicError' ()) Unit)
deleteR_ req url = simpleRequest_ (Left DELETE) req url (Nothing :: Maybe Foreign)

-- | Makes a `DELETE` request, taking an `URL`,
-- | ignoring the response payload.
delete_ ::
  URL ->
  Aff (Either (BasicError' ()) Unit)
delete_ = deleteR_ defaultSimpleRequest
