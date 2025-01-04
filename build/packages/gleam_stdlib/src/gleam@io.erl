-module(gleam@io).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([print/1, print_error/1, println/1, println_error/1, debug/1]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/io.gleam", 17).
-spec print(binary()) -> nil.
print(String) ->
    gleam_stdlib:print(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/io.gleam", 33).
-spec print_error(binary()) -> nil.
print_error(String) ->
    gleam_stdlib:print_error(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/io.gleam", 47).
-spec println(binary()) -> nil.
println(String) ->
    gleam_stdlib:println(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/io.gleam", 61).
-spec println_error(binary()) -> nil.
println_error(String) ->
    gleam_stdlib:println_error(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/io.gleam", 96).
-spec debug(EJD) -> EJD.
debug(Term) ->
    _pipe = Term,
    _pipe@1 = gleam@string:inspect(_pipe),
    gleam_stdlib:println_error(_pipe@1),
    Term.
