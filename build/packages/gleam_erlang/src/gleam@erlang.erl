-module(gleam@erlang).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([format/1, term_to_binary/1, get_line/1, system_time/1, erlang_timestamp/0, rescue/1, binary_to_term/1, unsafe_binary_to_term/1, start_arguments/0, ensure_all_started/1, make_reference/0, reference_from_dynamic/1, priv_directory/1, bounded_phash2/2, phash2/1]).
-export_type([safe/0, get_line_error/0, time_unit/0, crash/0, ensure_all_started_error/0, reference_/0]).

-type safe() :: safe.

-type get_line_error() :: eof | no_data.

-type time_unit() :: second | millisecond | microsecond | nanosecond.

-type crash() :: {exited, gleam@dynamic:dynamic_()} |
    {thrown, gleam@dynamic:dynamic_()} |
    {errored, gleam@dynamic:dynamic_()}.

-type ensure_all_started_error() :: {unknown_application,
        gleam@erlang@atom:atom_()} |
    {application_failed_to_start,
        gleam@erlang@atom:atom_(),
        gleam@dynamic:dynamic_()}.

-type reference_() :: any().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 17).
-spec format(any()) -> binary().
format(Term) ->
    unicode:characters_to_binary(io_lib:format(<<"~p"/utf8>>, [Term])).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 27).
-spec term_to_binary(any()) -> bitstring().
term_to_binary(A) ->
    erlang:term_to_binary(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 80).
-spec get_line(binary()) -> {ok, binary()} | {error, get_line_error()}.
get_line(Prompt) ->
    gleam_erlang_ffi:get_line(Prompt).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 93).
-spec system_time(time_unit()) -> integer().
system_time(A) ->
    os:system_time(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 99).
-spec erlang_timestamp() -> {integer(), integer(), integer()}.
erlang_timestamp() ->
    os:timestamp().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 109).
-spec rescue(fun(() -> FLH)) -> {ok, FLH} | {error, crash()}.
rescue(A) ->
    gleam_erlang_ffi:rescue(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 41).
-spec binary_to_term(bitstring()) -> {ok, gleam@dynamic:dynamic_()} |
    {error, nil}.
binary_to_term(Binary) ->
    case gleam_erlang_ffi:rescue(
        fun() -> erlang:binary_to_term(Binary, [safe]) end
    ) of
        {ok, Term} ->
            {ok, Term};

        {error, _} ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 56).
-spec unsafe_binary_to_term(bitstring()) -> {ok, gleam@dynamic:dynamic_()} |
    {error, nil}.
unsafe_binary_to_term(Binary) ->
    case gleam_erlang_ffi:rescue(fun() -> erlang:binary_to_term(Binary, []) end) of
        {ok, Term} ->
            {ok, Term};

        {error, _} ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 124).
-spec start_arguments() -> list(binary()).
start_arguments() ->
    _pipe = init:get_plain_arguments(),
    gleam@list:map(_pipe, fun unicode:characters_to_binary/1).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 147).
-spec ensure_all_started(gleam@erlang@atom:atom_()) -> {ok,
        list(gleam@erlang@atom:atom_())} |
    {error, ensure_all_started_error()}.
ensure_all_started(Application) ->
    gleam_erlang_ffi:ensure_all_started(Application).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 170).
-spec make_reference() -> reference_().
make_reference() ->
    erlang:make_ref().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 191).
-spec reference_from_dynamic(gleam@dynamic:dynamic_()) -> {ok, reference_()} |
    {error, list(gleam@dynamic:decode_error())}.
reference_from_dynamic(From) ->
    gleam_erlang_ffi:reference_from_dynamic(From).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 208).
-spec priv_directory(binary()) -> {ok, binary()} | {error, nil}.
priv_directory(Name) ->
    gleam_erlang_ffi:priv_directory(Name).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 219).
-spec bounded_phash2(any(), integer()) -> integer().
bounded_phash2(Term, Limit) ->
    erlang:phash2(Term, Limit).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 223).
-spec phash2(any()) -> integer().
phash2(Term) ->
    erlang:phash2(Term).
