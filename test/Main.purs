module Test.Main where

import Prelude

import Affjax as AX
import Affjax.ResponseHeader (ResponseHeader, name)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (throwError)
import Data.Array (filter, null)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant (default, on)
import Effect (Effect)
import Effect.Aff (Aff, finally, forkAff, killFiber, runAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Class.Console as A
import Effect.Exception (error, throwException)
import Prim.Row as Row
import Simple.Ajax as SA
import Simple.Ajax.Errors as SAE
import Type.Prelude (SProxy)

foreign import logAny :: forall a. a -> Effect Unit

foreign import data Server :: Type
foreign import startServer :: EffectFnAff { server :: Server, port :: Int }
foreign import stopServer :: Server -> EffectFnAff Unit

logAny' :: forall a. a -> Aff Unit
logAny' = liftEffect <<< logAny

assertFail :: forall a. String -> Aff a
assertFail = throwError <<< error

assertMsg :: String -> Boolean -> Aff Unit
assertMsg _ true = pure unit
assertMsg msg false = assertFail msg

assertRight :: forall a b. Either a b -> Aff b
assertRight x = case x of
  Left y -> logAny' y >>= \_ -> assertFail "Expected a Right value"
  Right y -> pure y

assertLeft :: forall a. Either a Boolean -> Aff a
assertLeft x = case x of
  Right y -> logAny' y >>= \_ -> assertFail "Expected a Left value"
  Left y -> pure y

assertEq :: forall a. Eq a => Show a => a -> a -> Aff Unit
assertEq x y =
  when (x /= y) $ assertFail $ "Expected " <> show x <> ", got " <> show y

assertError ::
  forall sym a t r.
  IsSymbol sym =>
  Row.Cons sym a t (SAE.BasicErrorRow r) =>
  SProxy sym ->
  SAE.BasicError r ->
  Aff Unit
assertError s err = default (assertFail "Expected a different status error")
                # on s (const $ pure unit)
                $ err

type UserRequest = {username:: String, password :: String}

main :: Effect Unit
main = void $ runAff (either (\e -> logShow e *> throwException e) (const $ log "affjax: All good!")) do
  let ok200 = StatusCode 200
  let notFound404 = StatusCode 404

  -- let retryPolicy = AX.defaultRetryPolicy { timeout = Just (Milliseconds 500.0)
  --                                         , shouldRetryWithStatusCode = \_ -> true
  --                                         }

  { server, port } ← fromEffectFnAff startServer
  finally (fromEffectFnAff (stopServer server)) do
    A.log ("Test server running on port " <> show port)

    let prefix = append ("http://localhost:" <> show port)
    let register = prefix "/register"
    let mirror = prefix "/mirror"
    let putUrl = prefix "/put"
    let timedFailsUrl = prefix "/timed_fails"
    let unauthorized = prefix "/unauthorized"
    let doesNotExist = prefix "/does-not-exist"
    let notJson = prefix "/not-json"

    -- A.log "GET /does-not-exist: should be 404 Not found after retries"
    -- SA.getR { retryPolicy: Just retryPolicy } doesNotExist >>= assertLeft >>= \e -> do
    --   assertError SAE._notFound e

    A.log "GET /does-not-exist: should be 404 Not found"
    SA.get doesNotExist >>= assertLeft >>= \e -> do
      assertError SAE._notFound e

    A.log "POST /unauthorized: should return 404 Unauthoized"
    SA.post unauthorized (Just 1) >>= assertLeft >>= \e -> do
      assertError SAE._unAuthorized e

    A.log "GET /mirror: should be 200 OK"
    void $ SA.get mirror >>= \(v :: Either SAE.AjaxError Boolean) -> assertRight v

    A.log "GET /not-json: invalid JSON should return an error"
    SA.get notJson >>= assertLeft >>= \e -> do
      assertError SAE._parseError e

    A.log "POST /register: should return a response header"
    SA.postH_ register (Just {username : "test", password : "test"}) >>= assertRight >>= \(v :: (Tuple (Array ResponseHeader) Unit)) ->
      case v of
        (Tuple headers _) ->
          assertMsg "Response headers did not contain expected header" (null $ filter (\h -> (name h) == "Authorization") headers)

    A.log "POST /mirror: should use the POST method"
    SA.post mirror (Just { foo: "test" }) >>= assertRight >>= \res -> do
      assertEq res { foo: "test" }

    A.log "PUT with a request body"
    let content = "the quick brown fox jumps over the lazy dog"
    SA.put putUrl (Just content) >>= assertRight >>= \res -> do
      assertEq res { foo: 1, bar: content }

    -- A.log "DELETE on errors with retry"
    -- SA.deleteR { retryPolicy: Just retryPolicy } timedFailsUrl >>= assertRight >>= \res -> do
    --   assertEq res { foo: "bar" }

    A.log "Testing CORS, HTTPS"
    SA.get "https://cors-test.appspot.com/test" >>= assertRight >>= \res -> do
      assertEq res { status: "ok" }

    A.log "Testing cancellation"
    forkAff (SA.post_ mirror (Just "do it now")) >>= killFiber (error "Pull the cord!")
    assertMsg "should have been canceled" true
