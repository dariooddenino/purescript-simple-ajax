# Purescript-Simple-Ajax

An opinionated library to work with AJAX and JSON, using [`simple-json`](https://pursuit.purescript.org/packages/purescript-simple-json) and [`variant`](https://pursuit.purescript.org/packages/purescript-variant).

A more detailed guide can be found [here](http://codingstruggles.com/purescript/purescript-simple-ajax.html).


Thanks to Vladimir Ciobanu for his [error handling solution](https://github.com/vladciobanu/purescript-affjax-errors).

## Functions

All requests have 4 versions:
- `post`: Takes the request `URL` and some optional content and then tries to parse the response.
- `post_`: Takes the request `URL` and some optional content, but ignores the response payload.
- `postR`: Like `post`, but takes a subset of a `SimpleRequest` as an additional argument (for example if additional headers are needed).
- `postR_`: Like `post_`, but takes a subset of a `SimpleRequest` as an additional argument.

`POST` requests also have variations that includes the response headers. `(Tuple (Array ResponseHeader) b)` is returned in place of `b`, where `b` is just `Unit` in the `_` versions: `postH`, `postH_`, `postRH`, `postRH_`

Requests payload objects must implement an instance of `WriteForeign` and responses payload objects must implement an instance of `ReadForeign`.

Check [simple-json](https://github.com/justinwoo/purescript-simple-json) documentation to learn more about this.

## Requests

`simpleRequest`, `getR`, `postR`, `postRH`, `putR`, `deleteR` and `patchR` (and the
versions ending with an underscore) accept a subset of a `SimpleRequest` as
an argument. 

```purs
type SimpleRequest = { headers         :: Array RequestHeader
                     , username        :: Maybe String
                     , password        :: Maybe String
                     , withCredentials :: Boolean
                     }
```

For example:

```purs
getR { withCredentials: true } "http://www.google.it"
```

### Headers and MediaType

The default requests sets the header `Accept (MediaType "application/json")`.
When passing a new headers array, this header should be included again or the request will fail.

## Errors

The different types of error (`Error`, `ForeignError` and `ResponseFormatError`) are put together in a `Variant`.

There are two type alias:
- `HTTPError` containing the common http errors
- `AjaxError` which extends `HTTPError` to add json parsing errors

By using `Variant`'s functions, it's possible to match on them:

```purs
let error = 
  default "Generic error"
  # on _notFound $ const "Not found"
  # on _badRequest identity
  # on _parseError $ intercalate ", " <<< map renderForeignError
  $ err
```

## Example usage

```purs
import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (default, on)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)
import Simple.Ajax (_unAuthorized, post)


payload :: { foo :: Int, bar :: String }
payload = { foo: 1, bar: "hello" }

type Baz = { baz :: Boolean }

main = launchAff_ $ do
  res <- post url (Just payload)
  case res of
    Left err -> do
      let error = 
            default "Generic error" 
            # on _unAuthorized (const "Not authorized") 
            $ err
      log error
    Right (res :: Baz) ->
      logShow res
```

**NOTE**: To run this in the console you will need to install `xhr2` (or similar) with `npm`.
```
npm init
npm install xhr2
spago run
```

## Module documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-simple-ajax).
