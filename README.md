# servant-exceptions
Servant servers typically run their handlers in some form of `IO`. Either directly in the builtin `Handler` monad or a custom monad transformer on top it. When APIs fail, one would typically use the `MonadError ServantError` instance via `throwError` to create an error response of type `ServantErr`.

This approach has two problems:

* `Handler` (basically being `ExceptT ServantErr IO`) is considered an anti-pattern by some, as it suggests to novice users that only `ServantErr` would occur, but in `IO` any exception can be raised to abort execution
* `ServantErr` values need to be created at the call site of `throwError`, where the requested content type and/or headers are not available

`servant-exception` tries to help with both by making it easy to catch specific error types with an instance of `Exception` and provide automatic encoding into the requested content-type.

The API combinator `Throws e` can be used to catch exceptions of type `e` in the server, for example:

```
type API = "api" :> Throws UsersError :> "users" :> Get '[JSON, PlainText] [User]
```

The type `UsersError` can then be used to describe expected errors and their conversion via type class instances:

```
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

## TODO

This package is still a work-in-progress and lacks at least

* Documentation, more examples
* `servant-client` support
* `servant-docs` support

## Credit

This package is inspired by `servant-checked-exceptions` (Throws combinator) and
the generalized error handling in `cardano-sl`.
