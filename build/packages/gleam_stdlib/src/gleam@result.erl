-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, nil_error/1, values/1, try_recover/2]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 20).
-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 41).
-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 66).
-spec map({ok, BYB} | {error, BYC}, fun((BYB) -> BYF)) -> {ok, BYF} |
    {error, BYC}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 91).
-spec map_error({ok, BYI} | {error, BYJ}, fun((BYJ) -> BYM)) -> {ok, BYI} |
    {error, BYM}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 120).
-spec flatten({ok, {ok, BYP} | {error, BYQ}} | {error, BYQ}) -> {ok, BYP} |
    {error, BYQ}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 158).
-spec 'try'({ok, BYX} | {error, BYY}, fun((BYX) -> {ok, BZB} | {error, BYY})) -> {ok,
        BZB} |
    {error, BYY}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 170).
-spec then({ok, BZG} | {error, BZH}, fun((BZG) -> {ok, BZK} | {error, BZH})) -> {ok,
        BZK} |
    {error, BZH}.
then(Result, Fun) ->
    'try'(Result, Fun).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 192).
-spec unwrap({ok, BZP} | {error, any()}, BZP) -> BZP.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 214).
-spec lazy_unwrap({ok, BZT} | {error, any()}, fun(() -> BZT)) -> BZT.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 236).
-spec unwrap_error({ok, any()} | {error, BZY}, BZY) -> BZY.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 258).
-spec unwrap_both({ok, CAB} | {error, CAB}) -> CAB.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 308).
-spec 'or'({ok, CAK} | {error, CAL}, {ok, CAK} | {error, CAL}) -> {ok, CAK} |
    {error, CAL}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 341).
-spec lazy_or({ok, CAS} | {error, CAT}, fun(() -> {ok, CAS} | {error, CAT})) -> {ok,
        CAS} |
    {error, CAT}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 367).
-spec all(list({ok, CBA} | {error, CBB})) -> {ok, list(CBA)} | {error, CBB}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 387).
-spec partition_loop(list({ok, CBP} | {error, CBQ}), list(CBP), list(CBQ)) -> {list(CBP),
    list(CBQ)}.
partition_loop(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            partition_loop(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            partition_loop(Rest@1, Oks, [E | Errors])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 383).
-spec partition(list({ok, CBI} | {error, CBJ})) -> {list(CBI), list(CBJ)}.
partition(Results) ->
    partition_loop(Results, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 409).
-spec replace({ok, any()} | {error, CBY}, CCB) -> {ok, CCB} | {error, CBY}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 430).
-spec replace_error({ok, CCE} | {error, any()}, CCI) -> {ok, CCE} | {error, CCI}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 280).
-spec nil_error({ok, CAE} | {error, any()}) -> {ok, CAE} | {error, nil}.
nil_error(Result) ->
    replace_error(Result, nil).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 446).
-spec values(list({ok, CCL} | {error, any()})) -> list(CCL).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 479).
-spec try_recover(
    {ok, CCR} | {error, CCS},
    fun((CCS) -> {ok, CCR} | {error, CCV})
) -> {ok, CCR} | {error, CCV}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
