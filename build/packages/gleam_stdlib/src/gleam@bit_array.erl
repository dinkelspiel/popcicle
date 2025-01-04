-module(gleam@bit_array).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([from_string/1, byte_size/1, bit_size/1, pad_to_bytes/1, slice/3, is_utf8/1, to_string/1, concat/1, append/2, base64_encode/2, base64_decode/1, base64_url_encode/2, base64_url_decode/1, base16_encode/1, base16_decode/1, inspect/1, compare/2, starts_with/2]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 11).
-spec from_string(binary()) -> bitstring().
from_string(X) ->
    gleam_stdlib:identity(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 24).
-spec byte_size(bitstring()) -> integer().
byte_size(X) ->
    erlang:byte_size(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 16).
-spec bit_size(bitstring()) -> integer().
bit_size(X) ->
    erlang:bit_size(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 29).
-spec pad_to_bytes(bitstring()) -> bitstring().
pad_to_bytes(X) ->
    gleam_stdlib:bit_array_pad_to_bytes(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 56).
-spec slice(bitstring(), integer(), integer()) -> {ok, bitstring()} |
    {error, nil}.
slice(String, Position, Length) ->
    gleam_stdlib:bit_array_slice(String, Position, Length).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 69).
-spec is_utf8_loop(bitstring()) -> boolean().
is_utf8_loop(Bits) ->
    case Bits of
        <<>> ->
            true;

        <<_/utf8, Rest/binary>> ->
            is_utf8_loop(Rest);

        _ ->
            false
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 64).
-spec is_utf8(bitstring()) -> boolean().
is_utf8(Bits) ->
    is_utf8_loop(Bits).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 90).
-spec to_string(bitstring()) -> {ok, binary()} | {error, nil}.
to_string(Bits) ->
    case is_utf8(Bits) of
        true ->
            {ok, gleam_stdlib:identity(Bits)};

        false ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 111).
-spec concat(list(bitstring())) -> bitstring().
concat(Bit_arrays) ->
    gleam_stdlib:bit_array_concat(Bit_arrays).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 42).
-spec append(bitstring(), bitstring()) -> bitstring().
append(First, Second) ->
    gleam_stdlib:bit_array_concat([First, Second]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 120).
-spec base64_encode(bitstring(), boolean()) -> binary().
base64_encode(Input, Padding) ->
    gleam_stdlib:bit_array_base64_encode(Input, Padding).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 124).
-spec base64_decode(binary()) -> {ok, bitstring()} | {error, nil}.
base64_decode(Encoded) ->
    Padded = case erlang:byte_size(gleam_stdlib:identity(Encoded)) rem 4 of
        0 ->
            Encoded;

        N ->
            gleam@string:append(
                Encoded,
                gleam@string:repeat(<<"="/utf8>>, 4 - N)
            )
    end,
    gleam_stdlib:base_decode64(Padded).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 142).
-spec base64_url_encode(bitstring(), boolean()) -> binary().
base64_url_encode(Input, Padding) ->
    _pipe = gleam_stdlib:bit_array_base64_encode(Input, Padding),
    _pipe@1 = gleam@string:replace(_pipe, <<"+"/utf8>>, <<"-"/utf8>>),
    gleam@string:replace(_pipe@1, <<"/"/utf8>>, <<"_"/utf8>>).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 151).
-spec base64_url_decode(binary()) -> {ok, bitstring()} | {error, nil}.
base64_url_decode(Encoded) ->
    _pipe = Encoded,
    _pipe@1 = gleam@string:replace(_pipe, <<"-"/utf8>>, <<"+"/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"_"/utf8>>, <<"/"/utf8>>),
    base64_decode(_pipe@2).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 165).
-spec base16_encode(bitstring()) -> binary().
base16_encode(Input) ->
    gleam_stdlib:base16_encode(Input).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 171).
-spec base16_decode(binary()) -> {ok, bitstring()} | {error, nil}.
base16_decode(Input) ->
    gleam_stdlib:base16_decode(Input).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 190).
-spec inspect_loop(bitstring(), binary()) -> binary().
inspect_loop(Input, Accumulator) ->
    case Input of
        <<>> ->
            Accumulator;

        <<X:1>> ->
            <<<<Accumulator/binary, (erlang:integer_to_binary(X))/binary>>/binary,
                ":size(1)"/utf8>>;

        <<X@1:2>> ->
            <<<<Accumulator/binary, (erlang:integer_to_binary(X@1))/binary>>/binary,
                ":size(2)"/utf8>>;

        <<X@2:3>> ->
            <<<<Accumulator/binary, (erlang:integer_to_binary(X@2))/binary>>/binary,
                ":size(3)"/utf8>>;

        <<X@3:4>> ->
            <<<<Accumulator/binary, (erlang:integer_to_binary(X@3))/binary>>/binary,
                ":size(4)"/utf8>>;

        <<X@4:5>> ->
            <<<<Accumulator/binary, (erlang:integer_to_binary(X@4))/binary>>/binary,
                ":size(5)"/utf8>>;

        <<X@5:6>> ->
            <<<<Accumulator/binary, (erlang:integer_to_binary(X@5))/binary>>/binary,
                ":size(6)"/utf8>>;

        <<X@6:7>> ->
            <<<<Accumulator/binary, (erlang:integer_to_binary(X@6))/binary>>/binary,
                ":size(7)"/utf8>>;

        <<X@7, Rest/bitstring>> ->
            Suffix = case Rest of
                <<>> ->
                    <<""/utf8>>;

                _ ->
                    <<", "/utf8>>
            end,
            Accumulator@1 = <<<<Accumulator/binary,
                    (erlang:integer_to_binary(X@7))/binary>>/binary,
                Suffix/binary>>,
            inspect_loop(Rest, Accumulator@1);

        _ ->
            Accumulator
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 185).
-spec inspect(bitstring()) -> binary().
inspect(Input) ->
    <<(inspect_loop(Input, <<"<<"/utf8>>))/binary, ">>"/utf8>>.

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 232).
-spec compare(bitstring(), bitstring()) -> gleam@order:order().
compare(A, B) ->
    case {A, B} of
        {<<First_byte, First_rest/bitstring>>,
            <<Second_byte, Second_rest/bitstring>>} ->
            case {First_byte, Second_byte} of
                {F, S} when F > S ->
                    gt;

                {F@1, S@1} when F@1 < S@1 ->
                    lt;

                {_, _} ->
                    compare(First_rest, Second_rest)
            end;

        {<<>>, <<>>} ->
            eq;

        {_, <<>>} ->
            gt;

        {<<>>, _} ->
            lt;

        {First, Second} ->
            case {gleam_stdlib:bit_array_to_int_and_size(First),
                gleam_stdlib:bit_array_to_int_and_size(Second)} of
                {{A@1, _}, {B@1, _}} when A@1 > B@1 ->
                    gt;

                {{A@2, _}, {B@2, _}} when A@2 < B@2 ->
                    lt;

                {{_, Size_a}, {_, Size_b}} when Size_a > Size_b ->
                    gt;

                {{_, Size_a@1}, {_, Size_b@1}} when Size_a@1 < Size_b@1 ->
                    lt;

                {_, _} ->
                    eq
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/bit_array.gleam", 272).
-spec starts_with(bitstring(), bitstring()) -> boolean().
starts_with(Bits, Prefix) ->
    Prefix_size = erlang:bit_size(Prefix),
    case Bits of
        <<Pref:Prefix_size/bitstring, _/bitstring>> when Pref =:= Prefix ->
            true;

        _ ->
            false
    end.
