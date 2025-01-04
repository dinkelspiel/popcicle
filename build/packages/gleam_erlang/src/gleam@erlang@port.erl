-module(gleam@erlang@port).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([port_from_dynamic/1]).
-export_type([port_/0]).

-type port_() :: any().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/port.gleam", 26).
-spec port_from_dynamic(gleam@dynamic:dynamic_()) -> {ok, port_()} |
    {error, list(gleam@dynamic:decode_error())}.
port_from_dynamic(From) ->
    gleam_erlang_ffi:port_from_dynamic(From).
