-module(gleam@erlang@node).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([self/0, visible/0, connect/1, send/3, to_atom/1]).
-export_type([node_/0, do_not_leak/0, connect_error/0]).

-type node_() :: any().

-type do_not_leak() :: any().

-type connect_error() :: failed_to_connect | local_node_is_not_alive.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/node.gleam", 10).
-spec self() -> node_().
self() ->
    erlang:node().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/node.gleam", 23).
-spec visible() -> list(node_()).
visible() ->
    erlang:nodes().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/node.gleam", 44).
-spec connect(gleam@erlang@atom:atom_()) -> {ok, node_()} |
    {error, connect_error()}.
connect(Node) ->
    gleam_erlang_ffi:connect_node(Node).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/node.gleam", 51).
-spec send(node_(), gleam@erlang@atom:atom_(), any()) -> nil.
send(Node, Name, Message) ->
    erlang:send({Name, Node}, Message),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/node.gleam", 62).
-spec to_atom(node_()) -> gleam@erlang@atom:atom_().
to_atom(Node) ->
    gleam_erlang_ffi:identity(Node).
