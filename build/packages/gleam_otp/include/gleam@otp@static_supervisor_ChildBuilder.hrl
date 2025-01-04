-record(child_builder, {
    id :: binary(),
    starter :: fun(() -> {ok, gleam@erlang@process:pid_()} |
        {error, gleam@dynamic:dynamic_()}),
    restart :: gleam@otp@static_supervisor:restart(),
    significant :: boolean(),
    child_type :: gleam@otp@static_supervisor:child_type()
}).
