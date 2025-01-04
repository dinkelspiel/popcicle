-module(gleam@int).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([absolute_value/1, parse/1, base_parse/2, to_string/1, to_base_string/2, to_base2/1, to_base8/1, to_base16/1, to_base36/1, to_float/1, power/2, square_root/1, compare/2, min/2, max/2, clamp/3, is_even/1, is_odd/1, negate/1, sum/1, product/1, digits/2, undigits/2, random/1, divide/2, remainder/2, modulo/2, floor_divide/2, add/2, multiply/2, subtract/2, bitwise_and/2, bitwise_not/1, bitwise_or/2, bitwise_exclusive_or/2, bitwise_shift_left/2, bitwise_shift_right/2]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 30).
-spec absolute_value(integer()) -> integer().
absolute_value(X) ->
    case X >= 0 of
        true ->
            X;

        false ->
            X * -1
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 109).
-spec parse(binary()) -> {ok, integer()} | {error, nil}.
parse(String) ->
    gleam_stdlib:parse_int(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 141).
-spec base_parse(binary(), integer()) -> {ok, integer()} | {error, nil}.
base_parse(String, Base) ->
    case (Base >= 2) andalso (Base =< 36) of
        true ->
            gleam_stdlib:int_from_base_string(String, Base);

        false ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 163).
-spec to_string(integer()) -> binary().
to_string(X) ->
    erlang:integer_to_binary(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 196).
-spec to_base_string(integer(), integer()) -> {ok, binary()} | {error, nil}.
to_base_string(X, Base) ->
    case (Base >= 2) andalso (Base =< 36) of
        true ->
            {ok, erlang:integer_to_binary(X, Base)};

        false ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 216).
-spec to_base2(integer()) -> binary().
to_base2(X) ->
    erlang:integer_to_binary(X, 2).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 229).
-spec to_base8(integer()) -> binary().
to_base8(X) ->
    erlang:integer_to_binary(X, 8).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 242).
-spec to_base16(integer()) -> binary().
to_base16(X) ->
    erlang:integer_to_binary(X, 16).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 255).
-spec to_base36(integer()) -> binary().
to_base36(X) ->
    erlang:integer_to_binary(X, 36).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 280).
-spec to_float(integer()) -> float().
to_float(X) ->
    erlang:float(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 67).
-spec power(integer(), float()) -> {ok, float()} | {error, nil}.
power(Base, Exponent) ->
    _pipe = Base,
    _pipe@1 = erlang:float(_pipe),
    gleam@float:power(_pipe@1, Exponent).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 87).
-spec square_root(integer()) -> {ok, float()} | {error, nil}.
square_root(X) ->
    _pipe = X,
    _pipe@1 = erlang:float(_pipe),
    gleam@float:square_root(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 316).
-spec compare(integer(), integer()) -> gleam@order:order().
compare(A, B) ->
    case A =:= B of
        true ->
            eq;

        false ->
            case A < B of
                true ->
                    lt;

                false ->
                    gt
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 336).
-spec min(integer(), integer()) -> integer().
min(A, B) ->
    case A < B of
        true ->
            A;

        false ->
            B
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 352).
-spec max(integer(), integer()) -> integer().
max(A, B) ->
    case A > B of
        true ->
            A;

        false ->
            B
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 291).
-spec clamp(integer(), integer(), integer()) -> integer().
clamp(X, Min_bound, Max_bound) ->
    _pipe = X,
    _pipe@1 = min(_pipe, Max_bound),
    max(_pipe@1, Min_bound).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 373).
-spec is_even(integer()) -> boolean().
is_even(X) ->
    (X rem 2) =:= 0.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 391).
-spec is_odd(integer()) -> boolean().
is_odd(X) ->
    (X rem 2) /= 0.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 404).
-spec negate(integer()) -> integer().
negate(X) ->
    -1 * X.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 421).
-spec sum_loop(list(integer()), integer()) -> integer().
sum_loop(Numbers, Initial) ->
    case Numbers of
        [X | Rest] ->
            sum_loop(Rest, X + Initial);

        [] ->
            Initial
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 417).
-spec sum(list(integer())) -> integer().
sum(Numbers) ->
    sum_loop(Numbers, 0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 444).
-spec product_loop(list(integer()), integer()) -> integer().
product_loop(Numbers, Initial) ->
    case Numbers of
        [X | Rest] ->
            product_loop(Rest, X * Initial);

        [] ->
            Initial
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 437).
-spec product(list(integer())) -> integer().
product(Numbers) ->
    case Numbers of
        [] ->
            1;

        _ ->
            product_loop(Numbers, 1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 473).
-spec digits_loop(integer(), integer(), list(integer())) -> list(integer()).
digits_loop(X, Base, Acc) ->
    case absolute_value(X) < Base of
        true ->
            [X | Acc];

        false ->
            digits_loop(case Base of
                    0 -> 0;
                    Gleam@denominator -> X div Gleam@denominator
                end, Base, [case Base of
                        0 -> 0;
                        Gleam@denominator@1 -> X rem Gleam@denominator@1
                    end | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 466).
-spec digits(integer(), integer()) -> {ok, list(integer())} | {error, nil}.
digits(X, Base) ->
    case Base < 2 of
        true ->
            {error, nil};

        false ->
            {ok, digits_loop(X, Base, [])}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 507).
-spec undigits_loop(list(integer()), integer(), integer()) -> {ok, integer()} |
    {error, nil}.
undigits_loop(Numbers, Base, Acc) ->
    case Numbers of
        [] ->
            {ok, Acc};

        [Digit | _] when Digit >= Base ->
            {error, nil};

        [Digit@1 | Rest] ->
            undigits_loop(Rest, Base, (Acc * Base) + Digit@1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 500).
-spec undigits(list(integer()), integer()) -> {ok, integer()} | {error, nil}.
undigits(Numbers, Base) ->
    case Base < 2 of
        true ->
            {error, nil};

        false ->
            undigits_loop(Numbers, Base, 0)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 536).
-spec random(integer()) -> integer().
random(Max) ->
    _pipe = (rand:uniform() * erlang:float(Max)),
    _pipe@1 = math:floor(_pipe),
    erlang:round(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 569).
-spec divide(integer(), integer()) -> {ok, integer()} | {error, nil}.
divide(Dividend, Divisor) ->
    case Divisor of
        0 ->
            {error, nil};

        Divisor@1 ->
            {ok, case Divisor@1 of
                    0 -> 0;
                    Gleam@denominator -> Dividend div Gleam@denominator
                end}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 621).
-spec remainder(integer(), integer()) -> {ok, integer()} | {error, nil}.
remainder(Dividend, Divisor) ->
    case Divisor of
        0 ->
            {error, nil};

        Divisor@1 ->
            {ok, case Divisor@1 of
                    0 -> 0;
                    Gleam@denominator -> Dividend rem Gleam@denominator
                end}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 663).
-spec modulo(integer(), integer()) -> {ok, integer()} | {error, nil}.
modulo(Dividend, Divisor) ->
    case Divisor of
        0 ->
            {error, nil};

        _ ->
            Remainder = case Divisor of
                0 -> 0;
                Gleam@denominator -> Dividend rem Gleam@denominator
            end,
            case (Remainder * Divisor) < 0 of
                true ->
                    {ok, Remainder + Divisor};

                false ->
                    {ok, Remainder}
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 707).
-spec floor_divide(integer(), integer()) -> {ok, integer()} | {error, nil}.
floor_divide(Dividend, Divisor) ->
    case Divisor of
        0 ->
            {error, nil};

        Divisor@1 ->
            case ((Dividend * Divisor@1) < 0) andalso ((case Divisor@1 of
                0 -> 0;
                Gleam@denominator -> Dividend rem Gleam@denominator
            end) /= 0) of
                true ->
                    {ok, (case Divisor@1 of
                            0 -> 0;
                            Gleam@denominator@1 -> Dividend div Gleam@denominator@1
                        end) - 1};

                false ->
                    {ok, case Divisor@1 of
                            0 -> 0;
                            Gleam@denominator@2 -> Dividend div Gleam@denominator@2
                        end}
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 741).
-spec add(integer(), integer()) -> integer().
add(A, B) ->
    A + B.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 769).
-spec multiply(integer(), integer()) -> integer().
multiply(A, B) ->
    A * B.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 802).
-spec subtract(integer(), integer()) -> integer().
subtract(A, B) ->
    A - B.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 814).
-spec bitwise_and(integer(), integer()) -> integer().
bitwise_and(X, Y) ->
    erlang:'band'(X, Y).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 824).
-spec bitwise_not(integer()) -> integer().
bitwise_not(X) ->
    erlang:'bnot'(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 834).
-spec bitwise_or(integer(), integer()) -> integer().
bitwise_or(X, Y) ->
    erlang:'bor'(X, Y).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 844).
-spec bitwise_exclusive_or(integer(), integer()) -> integer().
bitwise_exclusive_or(X, Y) ->
    erlang:'bxor'(X, Y).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 854).
-spec bitwise_shift_left(integer(), integer()) -> integer().
bitwise_shift_left(X, Y) ->
    erlang:'bsl'(X, Y).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 864).
-spec bitwise_shift_right(integer(), integer()) -> integer().
bitwise_shift_right(X, Y) ->
    erlang:'bsr'(X, Y).
