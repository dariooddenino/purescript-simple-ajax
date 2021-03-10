# CHANGELOG

## v4.0.0

### Replaced simple-json with argonaut

The main difference is that instead of `WriteForeign` and `ReadForeign` instances we now use `EncodeJson` and `DecodeJson`.
`Foreign` has been replaced with `Json`.

### New error type

The `HTTPError` type now has a `affjaxError` member which catches affjax errors which were previously squased into `serverError`.
