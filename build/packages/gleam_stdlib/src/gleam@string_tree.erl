-module(gleam@string_tree).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([append_tree/2, prepend_tree/2, from_strings/1, new/0, concat/1, from_string/1, prepend/2, append/2, to_string/1, byte_size/1, join/2, lowercase/1, uppercase/1, reverse/1, split/2, replace/3, is_equal/2, is_empty/1]).
-export_type([string_tree/0, direction/0]).

-type string_tree() :: any().

-type direction() :: all.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 61).
-spec append_tree(string_tree(), string_tree()) -> string_tree().
append_tree(Tree, Suffix) ->
    gleam_stdlib:iodata_append(Tree, Suffix).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 48).
-spec prepend_tree(string_tree(), string_tree()) -> string_tree().
prepend_tree(Tree, Prefix) ->
    gleam_stdlib:iodata_append(Prefix, Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 69).
-spec from_strings(list(binary())) -> string_tree().
from_strings(Strings) ->
    gleam_stdlib:identity(Strings).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 24).
-spec new() -> string_tree().
new() ->
    gleam_stdlib:identity([]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 77).
-spec concat(list(string_tree())) -> string_tree().
concat(Trees) ->
    gleam_stdlib:identity(Trees).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 85).
-spec from_string(binary()) -> string_tree().
from_string(String) ->
    gleam_stdlib:identity(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 32).
-spec prepend(string_tree(), binary()) -> string_tree().
prepend(Tree, Prefix) ->
    gleam_stdlib:iodata_append(gleam_stdlib:identity(Prefix), Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 40).
-spec append(string_tree(), binary()) -> string_tree().
append(Tree, Second) ->
    gleam_stdlib:iodata_append(Tree, gleam_stdlib:identity(Second)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 94).
-spec to_string(string_tree()) -> binary().
to_string(Tree) ->
    unicode:characters_to_binary(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 100).
-spec byte_size(string_tree()) -> integer().
byte_size(Tree) ->
    erlang:iolist_size(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 104).
-spec join(list(string_tree()), binary()) -> string_tree().
join(Trees, Sep) ->
    _pipe = Trees,
    _pipe@1 = gleam@list:intersperse(_pipe, gleam_stdlib:identity(Sep)),
    gleam_stdlib:identity(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 115).
-spec lowercase(string_tree()) -> string_tree().
lowercase(Tree) ->
    string:lowercase(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 122).
-spec uppercase(string_tree()) -> string_tree().
uppercase(Tree) ->
    string:uppercase(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 127).
-spec reverse(string_tree()) -> string_tree().
reverse(Tree) ->
    string:reverse(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 145).
-spec split(string_tree(), binary()) -> list(string_tree()).
split(Tree, Pattern) ->
    string:split(Tree, Pattern, all).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 156).
-spec replace(string_tree(), binary(), binary()) -> string_tree().
replace(Tree, Pattern, Substitute) ->
    gleam_stdlib:string_replace(Tree, Pattern, Substitute).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 182).
-spec is_equal(string_tree(), string_tree()) -> boolean().
is_equal(A, B) ->
    string:equal(A, B).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 206).
-spec is_empty(string_tree()) -> boolean().
is_empty(Tree) ->
    string:is_empty(Tree).
