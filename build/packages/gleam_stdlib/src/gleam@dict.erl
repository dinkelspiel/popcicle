-module(gleam@dict).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([size/1, to_list/1, new/0, is_empty/1, get/2, has_key/2, insert/3, from_list/1, keys/1, values/1, take/2, merge/2, delete/2, drop/2, upsert/3, fold/3, map_values/2, filter/2, each/2, combine/3]).
-export_type([dict/2]).

-type dict(KQ, KR) :: any() | {gleam_phantom, KQ, KR}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 36).
-spec size(dict(any(), any())) -> integer().
size(Dict) ->
    maps:size(Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 80).
-spec to_list(dict(LA, LB)) -> list({LA, LB}).
to_list(Dict) ->
    maps:to_list(Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 129).
-spec new() -> dict(any(), any()).
new() ->
    maps:new().

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 52).
-spec is_empty(dict(any(), any())) -> boolean().
is_empty(Dict) ->
    Dict =:= maps:new().

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 150).
-spec get(dict(MD, ME), MD) -> {ok, ME} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 116).
-spec has_key(dict(LR, any()), LR) -> boolean().
has_key(Dict, Key) ->
    maps:is_key(Key, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 169).
-spec insert(dict(MJ, MK), MJ, MK) -> dict(MJ, MK).
insert(Dict, Key, Value) ->
    maps:put(Key, Value, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 92).
-spec from_list_loop(list({LK, LL}), dict(LK, LL)) -> dict(LK, LL).
from_list_loop(List, Initial) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            from_list_loop(
                Rest,
                insert(Initial, erlang:element(1, X), erlang:element(2, X))
            )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 88).
-spec from_list(list({LF, LG})) -> dict(LF, LG).
from_list(List) ->
    maps:from_list(List).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 217).
-spec reverse_and_concat(list(NO), list(NO)) -> list(NO).
reverse_and_concat(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_and_concat(Rest, [Item | Accumulator])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 224).
-spec do_keys_loop(list({NS, any()}), list(NS)) -> list(NS).
do_keys_loop(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [First | Rest] ->
            do_keys_loop(Rest, [erlang:element(1, First) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 212).
-spec keys(dict(NJ, any())) -> list(NJ).
keys(Dict) ->
    maps:keys(Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 250).
-spec do_values_loop(list({any(), OD}), list(OD)) -> list(OD).
do_values_loop(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [First | Rest] ->
            do_values_loop(Rest, [erlang:element(2, First) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 245).
-spec values(dict(any(), NY)) -> list(NY).
values(Dict) ->
    maps:values(Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 319).
-spec do_take_loop(dict(PH, PI), list(PH), dict(PH, PI)) -> dict(PH, PI).
do_take_loop(Dict, Desired_keys, Acc) ->
    Insert = fun(Taken, Key) -> case gleam_stdlib:map_get(Dict, Key) of
            {ok, Value} ->
                insert(Taken, Key, Value);

            _ ->
                Taken
        end end,
    case Desired_keys of
        [] ->
            Acc;

        [First | Rest] ->
            do_take_loop(Dict, Rest, Insert(Acc, First))
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 310).
-spec take(dict(OT, OU), list(OT)) -> dict(OT, OU).
take(Dict, Desired_keys) ->
    maps:with(Desired_keys, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 357).
-spec insert_pair(dict(PY, PZ), {PY, PZ}) -> dict(PY, PZ).
insert_pair(Dict, Pair) ->
    insert(Dict, erlang:element(1, Pair), erlang:element(2, Pair)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 361).
-spec fold_inserts(list({QE, QF}), dict(QE, QF)) -> dict(QE, QF).
fold_inserts(New_entries, Dict) ->
    case New_entries of
        [] ->
            Dict;

        [First | Rest] ->
            fold_inserts(Rest, insert_pair(Dict, First))
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 351).
-spec merge(dict(PQ, PR), dict(PQ, PR)) -> dict(PQ, PR).
merge(Dict, New_entries) ->
    maps:merge(Dict, New_entries).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 383).
-spec delete(dict(QL, QM), QL) -> dict(QL, QM).
delete(Dict, Key) ->
    maps:remove(Key, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 411).
-spec drop(dict(QX, QY), list(QX)) -> dict(QX, QY).
drop(Dict, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Dict;

        [First | Rest] ->
            drop(delete(Dict, First), Rest)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 441).
-spec upsert(dict(RE, RF), RE, fun((gleam@option:option(RF)) -> RF)) -> dict(RE, RF).
upsert(Dict, Key, Fun) ->
    _pipe = Dict,
    _pipe@1 = gleam_stdlib:map_get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Dict, Key, _pipe@3).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 486).
-spec fold_loop(list({RQ, RR}), RT, fun((RT, RQ, RR) -> RT)) -> RT.
fold_loop(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            fold_loop(Rest, Fun(Initial, K, V), Fun)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 478).
-spec fold(dict(RL, RM), RP, fun((RP, RL, RM) -> RP)) -> RP.
fold(Dict, Initial, Fun) ->
    fold_loop(maps:to_list(Dict), Initial, Fun).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 188).
-spec map_values(dict(MV, MW), fun((MV, MW) -> MZ)) -> dict(MV, MZ).
map_values(Dict, Fun) ->
    maps:map(Fun, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 274).
-spec filter(dict(OH, OI), fun((OH, OI) -> boolean())) -> dict(OH, OI).
filter(Dict, Predicate) ->
    maps:filter(Predicate, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 519).
-spec each(dict(RU, RV), fun((RU, RV) -> any())) -> nil.
each(Dict, Fun) ->
    fold(
        Dict,
        nil,
        fun(Nil, K, V) ->
            Fun(K, V),
            Nil
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 540).
-spec combine(dict(RZ, SA), dict(RZ, SA), fun((SA, SA) -> SA)) -> dict(RZ, SA).
combine(Dict, Other, Fun) ->
    fold(
        Dict,
        Other,
        fun(Acc, Key, Value) -> case gleam_stdlib:map_get(Acc, Key) of
                {ok, Other_value} ->
                    insert(Acc, Key, Fun(Value, Other_value));

                {error, _} ->
                    insert(Acc, Key, Value)
            end end
    ).
