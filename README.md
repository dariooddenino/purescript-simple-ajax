# Purescript-Simple-Ajax

A very opinionated library to work with AJAX and JSON, using [`simple-json`](https://pursuit.purescript.org/packages/purescript-simple-json) and [`variant`](https://pursuit.purescript.org/packages/purescript-variant).

Thanks to Vladimir Ciobanu for his [error handling solution](https://github.com/vladciobanu/purescript-affjax-errors).

## Functions

All requests have 4 versions:
- `post`: Takes the request `URL` and some optional content and then tries to parse the response.
- `post_`: Takes the request `URL` and some optional content, but ignores the response payload.
- `postR`: Like `post`, but takes a `Request String` as an additional argument (for example if additional headers are needed).
- `postR_`: Like `post_`, but takes a `Request String` as an additional argument.

`get` and `getR` don't have a underscore variant.

Requests payload objects must implement an instance of `WriteForeign` and responses payload objects must implement an instance of `ReadForeign`.

Check [simple-json](https://github.com/justinwoo/purescript-simple-json) documentation to learn more about this.

## Errors

The different types of error (`Error`, `ForeignError` and `ResponseFormatError`) are put together in a `Variant`.

By using that library's function, it's possible to match on them:

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

payload :: { foo :: Int, bar :: String }
payload = { foo: 1, bar: "hello" }

type Baz = { baz :: Boolean }

main = launchAff_ $ do
  res <- post url (Just payload)
  case res of
    Left err -> do
      let error = 
        default "Generic error" 
        # on _notAuthorized $ const "Not authorized" 
        $ err
      log err
    Right (res :: Baz) ->
      logShow res
```

## Module documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-simple-ajax).
