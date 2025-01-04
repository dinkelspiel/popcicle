-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map2/3, index_map/2, try_map/2, drop/2, take/2, new/0, wrap/1, append/2, prepend/2, concat/1, flatten/1, flat_map/2, fold/3, count/2, group/2, map_fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1, max/2, sample/2]).
-export_type([continue_or_stop/1, sorting/0]).

-type continue_or_stop(YF) :: {continue, YF} | {stop, YF}.

-type sorting() :: ascending | descending.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 61).
-spec length_loop(list(any()), integer()) -> integer().
length_loop(List, Count) ->
    case List of
        [_ | List@1] ->
            length_loop(List@1, Count + 1);

        _ ->
            Count
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 57).
-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 130).
-spec reverse_loop(list(YP), list(YP)) -> list(YP).
reverse_loop(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_loop(Rest, [Item | Accumulator])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 126).
-spec reverse(list(YM)) -> list(YM).
reverse(List) ->
    lists:reverse(List).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 158).
-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 194).
-spec contains(list(YV), YV) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 221).
-spec first(list(YX)) -> {ok, YX} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 250).
-spec rest(list(AAB)) -> {ok, list(AAB)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Rest] ->
            {ok, Rest}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 292).
-spec update_group(fun((AAM) -> AAN)) -> fun((gleam@dict:dict(AAN, list(AAM)), AAM) -> gleam@dict:dict(AAN, list(AAM))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam_stdlib:map_get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@dict:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@dict:insert(Groups, F(Elem), [Elem])
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 320).
-spec filter_loop(list(AAX), fun((AAX) -> boolean()), list(AAX)) -> list(AAX).
filter_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                true ->
                    [First | Acc];

                false ->
                    Acc
            end,
            filter_loop(Rest, Fun, New_acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 316).
-spec filter(list(AAU), fun((AAU) -> boolean())) -> list(AAU).
filter(List, Predicate) ->
    filter_loop(List, Predicate, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 352).
-spec filter_map_loop(
    list(ABI),
    fun((ABI) -> {ok, ABK} | {error, any()}),
    list(ABK)
) -> list(ABK).
filter_map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                {ok, First@1} ->
                    [First@1 | Acc];

                {error, _} ->
                    Acc
            end,
            filter_map_loop(Rest, Fun, New_acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 348).
-spec filter_map(list(ABB), fun((ABB) -> {ok, ABD} | {error, any()})) -> list(ABD).
filter_map(List, Fun) ->
    filter_map_loop(List, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 383).
-spec map_loop(list(ABU), fun((ABU) -> ABW), list(ABW)) -> list(ABW).
map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            map_loop(Rest, Fun, [Fun(First) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 379).
-spec map(list(ABQ), fun((ABQ) -> ABS)) -> list(ABS).
map(List, Fun) ->
    map_loop(List, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 410).
-spec map2_loop(list(ACF), list(ACH), fun((ACF, ACH) -> ACJ), list(ACJ)) -> list(ACJ).
map2_loop(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            lists:reverse(Acc);

        {_, []} ->
            lists:reverse(Acc);

        {[A | As_], [B | Bs]} ->
            map2_loop(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 406).
-spec map2(list(ABZ), list(ACB), fun((ABZ, ACB) -> ACD)) -> list(ACD).
map2(List1, List2, Fun) ->
    map2_loop(List1, List2, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 465).
-spec index_map_loop(
    list(ACV),
    fun((ACV, integer()) -> ACX),
    integer(),
    list(ACX)
) -> list(ACX).
index_map_loop(List, Fun, Index, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            Acc@1 = [Fun(First, Index) | Acc],
            index_map_loop(Rest, Fun, Index + 1, Acc@1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 461).
-spec index_map(list(ACR), fun((ACR, integer()) -> ACT)) -> list(ACT).
index_map(List, Fun) ->
    index_map_loop(List, Fun, 0, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 519).
-spec try_map_loop(list(ADJ), fun((ADJ) -> {ok, ADL} | {error, ADM}), list(ADL)) -> {ok,
        list(ADL)} |
    {error, ADM}.
try_map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, lists:reverse(Acc)};

        [First | Rest] ->
            case Fun(First) of
                {ok, First@1} ->
                    try_map_loop(Rest, Fun, [First@1 | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 512).
-spec try_map(list(ADA), fun((ADA) -> {ok, ADC} | {error, ADD})) -> {ok,
        list(ADC)} |
    {error, ADD}.
try_map(List, Fun) ->
    try_map_loop(List, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 554).
-spec drop(list(ADT), integer()) -> list(ADT).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Rest] ->
                    drop(Rest, N - 1)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 589).
-spec take_loop(list(ADZ), integer(), list(ADZ)) -> list(ADZ).
take_loop(List, N, Acc) ->
    case N =< 0 of
        true ->
            lists:reverse(Acc);

        false ->
            case List of
                [] ->
                    lists:reverse(Acc);

                [First | Rest] ->
                    take_loop(Rest, N - 1, [First | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 585).
-spec take(list(ADW), integer()) -> list(ADW).
take(List, N) ->
    take_loop(List, N, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 609).
-spec new() -> list(any()).
new() ->
    [].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 629).
-spec wrap(AEF) -> list(AEF).
wrap(Item) ->
    [Item].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 650).
-spec append_loop(list(AEL), list(AEL)) -> list(AEL).
append_loop(First, Second) ->
    case First of
        [] ->
            Second;

        [Item | Rest] ->
            append_loop(Rest, [Item | Second])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 646).
-spec append(list(AEH), list(AEH)) -> list(AEH).
append(First, Second) ->
    lists:append(First, Second).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 670).
-spec prepend(list(AEP), AEP) -> list(AEP).
prepend(List, Item) ->
    [Item | List].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 675).
-spec reverse_and_prepend(list(AES), list(AES)) -> list(AES).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 698).
-spec concat_loop(list(list(AFA)), list(AFA)) -> list(AFA).
concat_loop(Lists, Acc) ->
    case Lists of
        [] ->
            lists:reverse(Acc);

        [List | Further_lists] ->
            concat_loop(Further_lists, reverse_and_prepend(List, Acc))
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 694).
-spec concat(list(list(AEW))) -> list(AEW).
concat(Lists) ->
    concat_loop(Lists, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 718).
-spec flatten(list(list(AFF))) -> list(AFF).
flatten(Lists) ->
    concat_loop(Lists, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 731).
-spec flat_map(list(AFJ), fun((AFJ) -> list(AFL))) -> list(AFL).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    flatten(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 744).
-spec fold(list(AFO), AFQ, fun((AFQ, AFO) -> AFQ)) -> AFQ.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 90).
-spec count(list(YK), fun((YK) -> boolean())) -> integer().
count(List, Predicate) ->
    fold(List, 0, fun(Acc, Value) -> case Predicate(Value) of
                true ->
                    Acc + 1;

                false ->
                    Acc
            end end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 288).
-spec group(list(AAG), fun((AAG) -> AAI)) -> gleam@dict:dict(AAI, list(AAG)).
group(List, Key) ->
    fold(List, maps:new(), update_group(Key)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 435).
-spec map_fold(list(ACM), ACO, fun((ACO, ACM) -> {ACO, ACP})) -> {ACO,
    list(ACP)}.
map_fold(List, Initial, Fun) ->
    _pipe = fold(
        List,
        {Initial, []},
        fun(Acc, Item) ->
            {Current_acc, Items} = Acc,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun lists:reverse/1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 766).
-spec fold_right(list(AFR), AFT, fun((AFT, AFR) -> AFT)) -> AFT.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 794).
-spec index_fold_loop(
    list(AFX),
    AFZ,
    fun((AFZ, AFX, integer()) -> AFZ),
    integer()
) -> AFZ.
index_fold_loop(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            index_fold_loop(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 786).
-spec index_fold(list(AFU), AFW, fun((AFW, AFU, integer()) -> AFW)) -> AFW.
index_fold(List, Initial, Fun) ->
    index_fold_loop(List, Initial, Fun, 0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 826).
-spec try_fold(list(AGA), AGC, fun((AGC, AGA) -> {ok, AGC} | {error, AGD})) -> {ok,
        AGC} |
    {error, AGD}.
try_fold(List, Initial, Fun) ->
    case List of
        [] ->
            {ok, Initial};

        [First | Rest] ->
            case Fun(Initial, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 865).
-spec fold_until(list(AGI), AGK, fun((AGK, AGI) -> continue_or_stop(AGK))) -> AGK.
fold_until(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [First | Rest] ->
            case Fun(Initial, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 902).
-spec find(list(AGM), fun((AGM) -> boolean())) -> {ok, AGM} | {error, nil}.
find(List, Is_desired) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 938).
-spec find_map(list(AGQ), fun((AGQ) -> {ok, AGS} | {error, any()})) -> {ok, AGS} |
    {error, nil}.
find_map(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 973).
-spec all(list(AGY), fun((AGY) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    all(Rest, Predicate);

                false ->
                    false
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1010).
-spec any(list(AHA), fun((AHA) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    true;

                false ->
                    any(Rest, Predicate)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1052).
-spec zip_loop(list(AHH), list(AHJ), list({AHH, AHJ})) -> list({AHH, AHJ}).
zip_loop(One, Other, Acc) ->
    case {One, Other} of
        {[First_one | Rest_one], [First_other | Rest_other]} ->
            zip_loop(Rest_one, Rest_other, [{First_one, First_other} | Acc]);

        {_, _} ->
            lists:reverse(Acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1048).
-spec zip(list(AHC), list(AHE)) -> list({AHC, AHE}).
zip(List, Other) ->
    zip_loop(List, Other, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1093).
-spec strict_zip_loop(list(AHU), list(AHW), list({AHU, AHW})) -> {ok,
        list({AHU, AHW})} |
    {error, nil}.
strict_zip_loop(One, Other, Acc) ->
    case {One, Other} of
        {[], []} ->
            {ok, lists:reverse(Acc)};

        {[], _} ->
            {error, nil};

        {_, []} ->
            {error, nil};

        {[First_one | Rest_one], [First_other | Rest_other]} ->
            strict_zip_loop(
                Rest_one,
                Rest_other,
                [{First_one, First_other} | Acc]
            )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1086).
-spec strict_zip(list(AHN), list(AHP)) -> {ok, list({AHN, AHP})} | {error, nil}.
strict_zip(List, Other) ->
    strict_zip_loop(List, Other, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1124).
-spec unzip_loop(list({AIH, AII}), list(AIH), list(AII)) -> {list(AIH),
    list(AII)}.
unzip_loop(Input, One, Other) ->
    case Input of
        [] ->
            {lists:reverse(One), lists:reverse(Other)};

        [{First_one, First_other} | Rest] ->
            unzip_loop(Rest, [First_one | One], [First_other | Other])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1120).
-spec unzip(list({AIC, AID})) -> {list(AIC), list(AID)}.
unzip(Input) ->
    unzip_loop(Input, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1159).
-spec intersperse_loop(list(AIR), AIR, list(AIR)) -> list(AIR).
intersperse_loop(List, Separator, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Rest] ->
            intersperse_loop(Rest, Separator, [X, Separator | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1152).
-spec intersperse(list(AIO), AIO) -> list(AIO).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            intersperse_loop(Rest, Elem, [X])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1177).
-spec unique(list(AIV)) -> list(AIV).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1258).
-spec sequences(
    list(AJB),
    fun((AJB, AJB) -> gleam@order:order()),
    list(AJB),
    sorting(),
    AJB,
    list(list(AJB))
) -> list(list(AJB)).
sequences(List, Compare, Growing, Direction, Prev, Acc) ->
    Growing@1 = [Prev | Growing],
    case List of
        [] ->
            case Direction of
                ascending ->
                    [reverse_loop(Growing@1, []) | Acc];

                descending ->
                    [Growing@1 | Acc]
            end;

        [New | Rest] ->
            case {Compare(Prev, New), Direction} of
                {gt, descending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {lt, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {eq, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {gt, ascending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [reverse_loop(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {lt, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [reverse_loop(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {eq, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [reverse_loop(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1406).
-spec merge_ascendings(
    list(AJY),
    list(AJY),
    fun((AJY, AJY) -> gleam@order:order()),
    list(AJY)
) -> list(AJY).
merge_ascendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            reverse_loop(List, Acc);

        {List, []} ->
            reverse_loop(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_ascendings(Rest1, List2, Compare, [First1 | Acc]);

                gt ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc]);

                eq ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1359).
-spec merge_ascending_pairs(
    list(list(AJM)),
    fun((AJM, AJM) -> gleam@order:order()),
    list(list(AJM))
) -> list(list(AJM)).
merge_ascending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            reverse_loop(Acc, []);

        [Sequence] ->
            reverse_loop([reverse_loop(Sequence, []) | Acc], []);

        [Ascending1, Ascending2 | Rest] ->
            Descending = merge_ascendings(Ascending1, Ascending2, Compare, []),
            merge_ascending_pairs(Rest, Compare, [Descending | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1433).
-spec merge_descendings(
    list(AKD),
    list(AKD),
    fun((AKD, AKD) -> gleam@order:order()),
    list(AKD)
) -> list(AKD).
merge_descendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            reverse_loop(List, Acc);

        {List, []} ->
            reverse_loop(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_descendings(List1, Rest2, Compare, [First2 | Acc]);

                gt ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc]);

                eq ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1381).
-spec merge_descending_pairs(
    list(list(AJS)),
    fun((AJS, AJS) -> gleam@order:order()),
    list(list(AJS))
) -> list(list(AJS)).
merge_descending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            reverse_loop(Acc, []);

        [Sequence] ->
            reverse_loop([reverse_loop(Sequence, []) | Acc], []);

        [Descending1, Descending2 | Rest] ->
            Ascending = merge_descendings(Descending1, Descending2, Compare, []),
            merge_descending_pairs(Rest, Compare, [Ascending | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1325).
-spec merge_all(
    list(list(AJI)),
    sorting(),
    fun((AJI, AJI) -> gleam@order:order())
) -> list(AJI).
merge_all(Sequences, Direction, Compare) ->
    case {Sequences, Direction} of
        {[], _} ->
            [];

        {[Sequence], ascending} ->
            Sequence;

        {[Sequence@1], descending} ->
            reverse_loop(Sequence@1, []);

        {_, ascending} ->
            Sequences@1 = merge_ascending_pairs(Sequences, Compare, []),
            merge_all(Sequences@1, descending, Compare);

        {_, descending} ->
            Sequences@2 = merge_descending_pairs(Sequences, Compare, []),
            merge_all(Sequences@2, ascending, Compare)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1196).
-spec sort(list(AIY), fun((AIY, AIY) -> gleam@order:order())) -> list(AIY).
sort(List, Compare) ->
    case List of
        [] ->
            [];

        [X] ->
            [X];

        [X@1, Y | Rest] ->
            Direction = case Compare(X@1, Y) of
                lt ->
                    ascending;

                eq ->
                    ascending;

                gt ->
                    descending
            end,
            Sequences = sequences(Rest, Compare, [X@1], Direction, Y, []),
            merge_all(Sequences, ascending, Compare)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1473).
-spec range_loop(integer(), integer(), list(integer())) -> list(integer()).
range_loop(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [Stop | Acc];

        gt ->
            range_loop(Start, Stop + 1, [Stop | Acc]);

        lt ->
            range_loop(Start, Stop - 1, [Stop | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1469).
-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    range_loop(Start, Stop, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1499).
-spec repeat_loop(AKN, integer(), list(AKN)) -> list(AKN).
repeat_loop(Item, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            repeat_loop(Item, Times - 1, [Item | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1495).
-spec repeat(AKL, integer()) -> list(AKL).
repeat(A, Times) ->
    repeat_loop(A, Times, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1532).
-spec split_loop(list(AKU), integer(), list(AKU)) -> {list(AKU), list(AKU)}.
split_loop(List, N, Taken) ->
    case N =< 0 of
        true ->
            {lists:reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {lists:reverse(Taken), []};

                [First | Rest] ->
                    split_loop(Rest, N - 1, [First | Taken])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1528).
-spec split(list(AKQ), integer()) -> {list(AKQ), list(AKQ)}.
split(List, Index) ->
    split_loop(List, Index, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1568).
-spec split_while_loop(list(ALD), fun((ALD) -> boolean()), list(ALD)) -> {list(ALD),
    list(ALD)}.
split_while_loop(List, F, Acc) ->
    case List of
        [] ->
            {lists:reverse(Acc), []};

        [First | Rest] ->
            case F(First) of
                false ->
                    {lists:reverse(Acc), List};

                _ ->
                    split_while_loop(Rest, F, [First | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1561).
-spec split_while(list(AKZ), fun((AKZ) -> boolean())) -> {list(AKZ), list(AKZ)}.
split_while(List, Predicate) ->
    split_while_loop(List, Predicate, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1608).
-spec key_find(list({ALI, ALJ}), ALI) -> {ok, ALJ} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1639).
-spec key_filter(list({ALN, ALO}), ALN) -> list(ALO).
key_filter(Keyword_list, Desired_key) ->
    filter_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1680).
-spec pop_loop(list(BEL), fun((BEL) -> boolean()), list(BEL)) -> {ok,
        {BEL, list(BEL)}} |
    {error, nil}.
pop_loop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, lists:append(lists:reverse(Checked), Rest)}};

                false ->
                    pop_loop(Rest, Predicate, [X | Checked])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1673).
-spec pop(list(ALR), fun((ALR) -> boolean())) -> {ok, {ALR, list(ALR)}} |
    {error, nil}.
pop(List, Is_desired) ->
    pop_loop(List, Is_desired, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1720).
-spec pop_map_loop(
    list(AMJ),
    fun((AMJ) -> {ok, AML} | {error, any()}),
    list(AMJ)
) -> {ok, {AML, list(AMJ)}} | {error, nil}.
pop_map_loop(List, Mapper, Checked) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, lists:append(lists:reverse(Checked), Rest)}};

                {error, _} ->
                    pop_map_loop(Rest, Mapper, [X | Checked])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1713).
-spec pop_map(list(AMA), fun((AMA) -> {ok, AMC} | {error, any()})) -> {ok,
        {AMC, list(AMA)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    pop_map_loop(Haystack, Is_desired, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1758).
-spec key_pop(list({AMT, AMU}), AMT) -> {ok, {AMU, list({AMT, AMU})}} |
    {error, nil}.
key_pop(List, Key) ->
    pop_map(
        List,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1785).
-spec key_set(list({AMZ, ANA}), AMZ, ANA) -> list({AMZ, ANA}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1807).
-spec each(list(AND), fun((AND) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [First | Rest] ->
            F(First),
            each(Rest, F)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1833).
-spec try_each(list(ANG), fun((ANG) -> {ok, any()} | {error, ANJ})) -> {ok, nil} |
    {error, ANJ}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [First | Rest] ->
            case Fun(First) of
                {ok, _} ->
                    try_each(Rest, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1866).
-spec partition_loop(list(BGQ), fun((BGQ) -> boolean()), list(BGQ), list(BGQ)) -> {list(BGQ),
    list(BGQ)}.
partition_loop(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {lists:reverse(Trues), lists:reverse(Falses)};

        [First | Rest] ->
            case Categorise(First) of
                true ->
                    partition_loop(Rest, Categorise, [First | Trues], Falses);

                false ->
                    partition_loop(Rest, Categorise, Trues, [First | Falses])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1859).
-spec partition(list(ANO), fun((ANO) -> boolean())) -> {list(ANO), list(ANO)}.
partition(List, Categorise) ->
    partition_loop(List, Categorise, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1886).
-spec permutations(list(ANX)) -> list(list(ANX)).
permutations(List) ->
    case List of
        [] ->
            [[]];

        _ ->
            _pipe@3 = index_map(
                List,
                fun(I, I_idx) ->
                    _pipe = index_fold(
                        List,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@1 = lists:reverse(_pipe),
                    _pipe@2 = permutations(_pipe@1),
                    map(_pipe@2, fun(Permutation) -> [I | Permutation] end)
                end
            ),
            flatten(_pipe@3)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1926).
-spec window_loop(list(list(AOF)), list(AOF), integer()) -> list(list(AOF)).
window_loop(Acc, List, N) ->
    Window = take(List, N),
    case erlang:length(Window) =:= N of
        true ->
            window_loop([Window | Acc], drop(List, 1), N);

        false ->
            lists:reverse(Acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1919).
-spec window(list(AOB), integer()) -> list(list(AOB)).
window(List, N) ->
    case N =< 0 of
        true ->
            [];

        false ->
            window_loop([], List, N)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1949).
-spec window_by_2(list(AOL)) -> list({AOL, AOL}).
window_by_2(List) ->
    zip(List, drop(List, 1)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1962).
-spec drop_while(list(AOO), fun((AOO) -> boolean())) -> list(AOO).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    drop_while(Rest, Predicate);

                false ->
                    [First | Rest]
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1992).
-spec take_while_loop(list(AOU), fun((AOU) -> boolean()), list(AOU)) -> list(AOU).
take_while_loop(List, Predicate, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    take_while_loop(Rest, Predicate, [First | Acc]);

                false ->
                    lists:reverse(Acc)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1985).
-spec take_while(list(AOR), fun((AOR) -> boolean())) -> list(AOR).
take_while(List, Predicate) ->
    take_while_loop(List, Predicate, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2024).
-spec chunk_loop(list(APD), fun((APD) -> APF), APF, list(APD), list(list(APD))) -> list(list(APD)).
chunk_loop(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                false ->
                    New_acc = [lists:reverse(Current_chunk) | Acc],
                    chunk_loop(Rest, F, Key, [First], New_acc);

                _ ->
                    chunk_loop(Rest, F, Key, [First | Current_chunk], Acc)
            end;

        _ ->
            lists:reverse([lists:reverse(Current_chunk) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2017).
-spec chunk(list(AOY), fun((AOY) -> any())) -> list(list(AOY)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            chunk_loop(Rest, F, F(First), [First], [])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2069).
-spec sized_chunk_loop(
    list(APP),
    integer(),
    integer(),
    list(APP),
    list(list(APP))
) -> list(list(APP)).
sized_chunk_loop(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    lists:reverse(Acc);

                Remaining ->
                    lists:reverse([lists:reverse(Remaining) | Acc])
            end;

        [First | Rest] ->
            Chunk = [First | Current_chunk],
            case Left > 1 of
                true ->
                    sized_chunk_loop(Rest, Count, Left - 1, Chunk, Acc);

                false ->
                    sized_chunk_loop(
                        Rest,
                        Count,
                        Count,
                        [],
                        [lists:reverse(Chunk) | Acc]
                    )
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2065).
-spec sized_chunk(list(APL), integer()) -> list(list(APL)).
sized_chunk(List, Count) ->
    sized_chunk_loop(List, Count, Count, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2113).
-spec reduce(list(APW), fun((APW, APW) -> APW)) -> {ok, APW} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2137).
-spec scan_loop(list(AQE), AQG, list(AQG), fun((AQG, AQE) -> AQG)) -> list(AQG).
scan_loop(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            lists:reverse(Accumulated);

        [First | Rest] ->
            Next = Fun(Accumulator, First),
            scan_loop(Rest, Next, [Next | Accumulated], Fun)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2129).
-spec scan(list(AQA), AQC, fun((AQC, AQA) -> AQC)) -> list(AQC).
scan(List, Initial, Fun) ->
    scan_loop(List, Initial, [], Fun).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2170).
-spec last(list(AQJ)) -> {ok, AQJ} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2189).
-spec combinations(list(AQN), integer()) -> list(list(AQN)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [First | Rest] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Rest, N - 1),
                            fun(Com) -> [First | Com] end
                        ),
                        lists:reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Rest, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2221).
-spec combination_pairs_loop(list(AQU)) -> list(list({AQU, AQU})).
combination_pairs_loop(Items) ->
    case Items of
        [] ->
            [];

        [First | Rest] ->
            First_combinations = map(Rest, fun(Other) -> {First, Other} end),
            [First_combinations | combination_pairs_loop(Rest)]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2216).
-spec combination_pairs(list(AQR)) -> list({AQR, AQR}).
combination_pairs(Items) ->
    _pipe = combination_pairs_loop(Items),
    flatten(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2258).
-spec transpose(list(list(ARC))) -> list(list(ARC)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Rest] ->
            transpose(Rest);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                flatten(_pipe@1)
            end,
            Rest@1 = transpose(
                map(Rows, fun(_capture) -> drop(_capture, 1) end)
            ),
            [Firsts | Rest@1]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2240).
-spec interleave(list(list(AQY))) -> list(AQY).
interleave(List) ->
    _pipe = transpose(List),
    flatten(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2299).
-spec shuffle_pair_unwrap_loop(list({float(), ARK}), list(ARK)) -> list(ARK).
shuffle_pair_unwrap_loop(List, Acc) ->
    case List of
        [] ->
            Acc;

        [Elem_pair | Enumerable] ->
            shuffle_pair_unwrap_loop(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2307).
-spec do_shuffle_by_pair_indexes(list({float(), ARO})) -> list({float(), ARO}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2292).
-spec shuffle(list(ARH)) -> list(ARH).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(_pipe, [], fun(Acc, A) -> [{rand:uniform(), A} | Acc] end),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    shuffle_pair_unwrap_loop(_pipe@2, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2329).
-spec max(list(ARR), fun((ARR, ARR) -> gleam@order:order())) -> {ok, ARR} |
    {error, nil}.
max(List, Compare) ->
    reduce(List, fun(Acc, Other) -> case Compare(Acc, Other) of
                gt ->
                    Acc;

                _ ->
                    Other
            end end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2402).
-spec log_random() -> float().
log_random() ->
    Min_positive = 2.2250738585072014e-308,
    _assert_subject = gleam@float:logarithm(rand:uniform() + Min_positive),
    {ok, Random} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam/list"/utf8>>,
                        function => <<"log_random"/utf8>>,
                        line => 2404})
    end,
    Random.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2376).
-spec sample_loop(
    list(ARZ),
    gleam@dict:dict(integer(), ARZ),
    integer(),
    integer(),
    float()
) -> gleam@dict:dict(integer(), ARZ).
sample_loop(List, Reservoir, K, Index, W) ->
    Skip = begin
        _assert_subject = gleam@float:logarithm(1.0 - W),
        {ok, Log_result} = case _assert_subject of
            {ok, _} -> _assert_subject;
            _assert_fail ->
                erlang:error(#{gleam_error => let_assert,
                            message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                            value => _assert_fail,
                            module => <<"gleam/list"/utf8>>,
                            function => <<"sample_loop"/utf8>>,
                            line => 2384})
        end,
        _pipe = case Log_result of
            +0.0 -> +0.0;
            -0.0 -> -0.0;
            Gleam@denominator -> log_random() / Gleam@denominator
        end,
        _pipe@1 = math:floor(_pipe),
        erlang:round(_pipe@1)
    end,
    Index@1 = (Index + Skip) + 1,
    case drop(List, Skip) of
        [] ->
            Reservoir;

        [Elem | Rest] ->
            Reservoir@1 = begin
                _pipe@2 = gleam@int:random(K),
                gleam@dict:insert(Reservoir, _pipe@2, Elem)
            end,
            W@1 = W * math:exp(case erlang:float(K) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator@1 -> log_random() / Gleam@denominator@1
                end),
            sample_loop(Rest, Reservoir@1, K, Index@1, W@1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2353).
-spec sample(list(ARV), integer()) -> list(ARV).
sample(List, K) ->
    case K =< 0 of
        true ->
            [];

        false ->
            {Reservoir, List@1} = split(List, K),
            case erlang:length(Reservoir) < K of
                true ->
                    Reservoir;

                false ->
                    Reservoir@1 = begin
                        _pipe = Reservoir,
                        _pipe@1 = map2(
                            range(0, K - 1),
                            _pipe,
                            fun(A, B) -> {A, B} end
                        ),
                        maps:from_list(_pipe@1)
                    end,
                    W = math:exp(case erlang:float(K) of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator -> log_random() / Gleam@denominator
                        end),
                    _pipe@2 = sample_loop(List@1, Reservoir@1, K, K, W),
                    maps:values(_pipe@2)
            end
    end.
