-module(gleam@erlang@charlist).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_string/1, from_string/1]).
-export_type([charlist/0]).

-type charlist() :: any().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/charlist.gleam", 14).
-spec to_string(charlist()) -> binary().
to_string(A) ->
    unicode:characters_to_binary(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/charlist.gleam", 22).
-spec from_string(binary()) -> charlist().
from_string(A) ->
    unicode:characters_to_list(A).
