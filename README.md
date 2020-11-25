# servant-exceptions [![Build Status](https://travis-ci.org/ch1bo/servant-exceptions.svg?branch=master)](https://travis-ci.org/ch1bo/servant-exceptions)

| Package                   | Hackage                                                                                                                                     |
| --------------------      | ---------------------------------------------------------------------------------------------------------------------------------           |
| servant-exceptions        | [![Hackage](https://img.shields.io/hackage/v/servant-exceptions.svg)](https://hackage.haskell.org/package/servant-exceptions)               |
| servant-exceptions-server | [![Hackage](https://img.shields.io/hackage/v/servant-exceptions-server.svg)](https://hackage.haskell.org/package/servant-exceptions-server) |

Servant servers typically run their handlers in some form of `IO`. Either directly in the builtin `Handler` monad or a custom monad transformer on top it. When APIs fail, one would typically use the `MonadError ServantError` instance via `throwError` to create an error response of type `ServantErr`.

This approach has two problems:

* `Handler` (basically being `ExceptT ServantErr IO`) is considered an anti-pattern by some, as it suggests to novice users that only `ServantErr` would occur, but in `IO` any exception can be raised to abort execution
* `ServantErr` values need to be created at the call site of `throwError`, where the requested content type and/or headers are not available

`servant-exceptions` tries to help with both by making it easy to catch specific error types with an instance of `Exception` and provide automatic encoding into the requested content-type.

The API combinator `Throws e` can be used to annotate what error types `e` might
be thrown by a server, for example:

```haskell
type API = "api" :> Throws UsersError :> "users" :> Get '[JSON, PlainText] [User]
```

The type `UsersError` can then be used to describe expected errors and their conversion via type class instances:

```haskell
data UsersError = UserNotFound
                | UserAlreadyExists
                | InternalError
                deriving (Show)

instance Exception UsersError

instance ToServantErr UsersError where
  status UserNotFound = status404
  status UserAlreadyExists = status409
  status InternalError = status500

instance ToJSON UsersError where
  toJSON e = object [ "type" .= show (typeOf e)
                    , "message" .= message e
                    ]

instance MimeRender PlainText UsersError where
  mimeRender ct = mimeRender ct . show
```

See [example](https://github.com/ch1bo/servant-exceptions/blob/master/example/Main.hs) for a full, commented example.

## Features

 * Declarative conversion of errors into error responses using `ToServantErr`
 * Respects `Accept` headers and constructs responses accordingly using `MimeRender`
 * Add headers to error responses, also via `ToServantErr`
 * Type-driven exception handling in `ServerT` stacks
 * Convert "backend" errors into "api" errors using `mapException`

## Planned things

This package lacks at least

* `servant-client` to rethrow exceptions (using `MonadThrow` and/or `MonadError`?)
* `servant-docs` support for automatic error documentation
* Documentation, more examples (explain included `ServantException` helper type)

## Credit

This package is inspired by `servant-checked-exceptions` (Throws combinator) and
the generalized error handling in `cardano-sl`.
