-module(gleam@otp@task).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([async/1, try_await/2, await/2, pid/1, await_forever/1, try_await2/3, try_await3/4, try_await4/5, try_await_all/2]).
-export_type([task/1, await_error/0, message2/2, message3/3, message4/4, message/1]).

-opaque task(HNB) :: {task,
        gleam@erlang@process:pid_(),
        gleam@erlang@process:pid_(),
        gleam@erlang@process:subject(HNB)}.

-type await_error() :: timeout | {exit, gleam@dynamic:dynamic_()}.

-type message2(HNC, HND) :: {m2_from_subject1, HNC} |
    {m2_from_subject2, HND} |
    m2_timeout.

-type message3(HNE, HNF, HNG) :: {m3_from_subject1, HNE} |
    {m3_from_subject2, HNF} |
    {m3_from_subject3, HNG} |
    m3_timeout.

-type message4(HNH, HNI, HNJ, HNK) :: {m4_from_subject1, HNH} |
    {m4_from_subject2, HNI} |
    {m4_from_subject3, HNJ} |
    {m4_from_subject4, HNK} |
    m4_timeout.

-type message(HNL) :: {message, integer(), HNL} | message_timeout.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 55).
-spec async(fun(() -> HNM)) -> task(HNM).
async(Work) ->
    Owner = erlang:self(),
    Subject = gleam@erlang@process:new_subject(),
    Pid = gleam@erlang@process:start(
        fun() -> gleam@erlang@process:send(Subject, Work()) end,
        true
    ),
    {task, Owner, Pid, Subject}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 70).
-spec assert_owner(task(any())) -> nil.
assert_owner(Task) ->
    Self = erlang:self(),
    case erlang:element(2, Task) =:= Self of
        true ->
            nil;

        false ->
            gleam@erlang@process:send_abnormal_exit(
                Self,
                <<"awaited on a task that does not belong to this process"/utf8>>
            )
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 88).
-spec try_await(task(HNQ), integer()) -> {ok, HNQ} | {error, await_error()}.
try_await(Task, Timeout) ->
    assert_owner(Task),
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        gleam@erlang@process:selecting(
            _pipe,
            erlang:element(4, Task),
            fun gleam@function:identity/1
        )
    end,
    case gleam_erlang_ffi:select(Selector, Timeout) of
        {ok, X} ->
            {ok, X};

        {error, nil} ->
            {error, timeout}
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 107).
-spec await(task(HNU), integer()) -> HNU.
await(Task, Timeout) ->
    _assert_subject = try_await(Task, Timeout),
    {ok, Value} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam/otp/task"/utf8>>,
                        function => <<"await"/utf8>>,
                        line => 108})
    end,
    Value.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 114).
-spec pid(task(any())) -> gleam@erlang@process:pid_().
pid(Task) ->
    erlang:element(3, Task).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 125).
-spec await_forever(task(HNY)) -> HNY.
await_forever(Task) ->
    assert_owner(Task),
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        gleam@erlang@process:selecting(
            _pipe,
            erlang:element(4, Task),
            fun gleam@function:identity/1
        )
    end,
    gleam_erlang_ffi:select(Selector).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 162).
-spec try_await2_loop(
    gleam@erlang@process:selector(message2(HOI, HOJ)),
    gleam@option:option({ok, HOI} | {error, await_error()}),
    gleam@option:option({ok, HOJ} | {error, await_error()}),
    gleam@erlang@process:timer()
) -> {{ok, HOI} | {error, await_error()}, {ok, HOJ} | {error, await_error()}}.
try_await2_loop(Selector, T1, T2, Timer) ->
    case {T1, T2} of
        {{some, T1@1}, {some, T2@1}} ->
            gleam@erlang@process:cancel_timer(Timer),
            {T1@1, T2@1};

        {_, _} ->
            case gleam_erlang_ffi:select(Selector) of
                {m2_from_subject1, X} ->
                    T1@2 = {some, {ok, X}},
                    try_await2_loop(Selector, T1@2, T2, Timer);

                {m2_from_subject2, X@1} ->
                    T2@2 = {some, {ok, X@1}},
                    try_await2_loop(Selector, T1, T2@2, Timer);

                m2_timeout ->
                    {gleam@option:unwrap(T1, {error, timeout}),
                        gleam@option:unwrap(T2, {error, timeout})}
            end
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 144).
-spec try_await2(task(HOA), task(HOC), integer()) -> {{ok, HOA} |
        {error, await_error()},
    {ok, HOC} | {error, await_error()}}.
try_await2(Task1, Task2, Timeout) ->
    assert_owner(Task1),
    assert_owner(Task2),
    Timeout_subject = gleam@erlang@process:new_subject(),
    Timer = gleam@erlang@process:send_after(
        Timeout_subject,
        Timeout,
        m2_timeout
    ),
    _pipe = gleam_erlang_ffi:new_selector(),
    _pipe@1 = gleam@erlang@process:selecting(
        _pipe,
        erlang:element(4, Task1),
        fun(Field@0) -> {m2_from_subject1, Field@0} end
    ),
    _pipe@2 = gleam@erlang@process:selecting(
        _pipe@1,
        erlang:element(4, Task2),
        fun(Field@0) -> {m2_from_subject2, Field@0} end
    ),
    _pipe@3 = gleam@erlang@process:selecting(
        _pipe@2,
        Timeout_subject,
        fun gleam@function:identity/1
    ),
    try_await2_loop(_pipe@3, none, none, Timer).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 230).
-spec try_await3_loop(
    gleam@erlang@process:selector(message3(HPJ, HPK, HPL)),
    gleam@option:option({ok, HPJ} | {error, await_error()}),
    gleam@option:option({ok, HPK} | {error, await_error()}),
    gleam@option:option({ok, HPL} | {error, await_error()}),
    gleam@erlang@process:timer()
) -> {{ok, HPJ} | {error, await_error()},
    {ok, HPK} | {error, await_error()},
    {ok, HPL} | {error, await_error()}}.
try_await3_loop(Selector, T1, T2, T3, Timer) ->
    case {T1, T2, T3} of
        {{some, T1@1}, {some, T2@1}, {some, T3@1}} ->
            gleam@erlang@process:cancel_timer(Timer),
            {T1@1, T2@1, T3@1};

        {_, _, _} ->
            case gleam_erlang_ffi:select(Selector) of
                {m3_from_subject1, X} ->
                    T1@2 = {some, {ok, X}},
                    try_await3_loop(Selector, T1@2, T2, T3, Timer);

                {m3_from_subject2, X@1} ->
                    T2@2 = {some, {ok, X@1}},
                    try_await3_loop(Selector, T1, T2@2, T3, Timer);

                {m3_from_subject3, X@2} ->
                    T3@2 = {some, {ok, X@2}},
                    try_await3_loop(Selector, T1, T2, T3@2, Timer);

                m3_timeout ->
                    {gleam@option:unwrap(T1, {error, timeout}),
                        gleam@option:unwrap(T2, {error, timeout}),
                        gleam@option:unwrap(T3, {error, timeout})}
            end
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 209).
-spec try_await3(task(HOX), task(HOZ), task(HPB), integer()) -> {{ok, HOX} |
        {error, await_error()},
    {ok, HOZ} | {error, await_error()},
    {ok, HPB} | {error, await_error()}}.
try_await3(Task1, Task2, Task3, Timeout) ->
    assert_owner(Task1),
    assert_owner(Task2),
    assert_owner(Task3),
    Timeout_subject = gleam@erlang@process:new_subject(),
    Timer = gleam@erlang@process:send_after(
        Timeout_subject,
        Timeout,
        m3_timeout
    ),
    _pipe = gleam_erlang_ffi:new_selector(),
    _pipe@1 = gleam@erlang@process:selecting(
        _pipe,
        erlang:element(4, Task1),
        fun(Field@0) -> {m3_from_subject1, Field@0} end
    ),
    _pipe@2 = gleam@erlang@process:selecting(
        _pipe@1,
        erlang:element(4, Task2),
        fun(Field@0) -> {m3_from_subject2, Field@0} end
    ),
    _pipe@3 = gleam@erlang@process:selecting(
        _pipe@2,
        erlang:element(4, Task3),
        fun(Field@0) -> {m3_from_subject3, Field@0} end
    ),
    _pipe@4 = gleam@erlang@process:selecting(
        _pipe@3,
        Timeout_subject,
        fun gleam@function:identity/1
    ),
    try_await3_loop(_pipe@4, none, none, none, Timer).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 312).
-spec try_await4_loop(
    gleam@erlang@process:selector(message4(HQV, HQW, HQX, HQY)),
    gleam@option:option({ok, HQV} | {error, await_error()}),
    gleam@option:option({ok, HQW} | {error, await_error()}),
    gleam@option:option({ok, HQX} | {error, await_error()}),
    gleam@option:option({ok, HQY} | {error, await_error()}),
    gleam@erlang@process:timer()
) -> {{ok, HQV} | {error, await_error()},
    {ok, HQW} | {error, await_error()},
    {ok, HQX} | {error, await_error()},
    {ok, HQY} | {error, await_error()}}.
try_await4_loop(Selector, T1, T2, T3, T4, Timer) ->
    case {T1, T2, T3, T4} of
        {{some, T1@1}, {some, T2@1}, {some, T3@1}, {some, T4@1}} ->
            gleam@erlang@process:cancel_timer(Timer),
            {T1@1, T2@1, T3@1, T4@1};

        {_, _, _, _} ->
            case gleam_erlang_ffi:select(Selector) of
                {m4_from_subject1, X} ->
                    T1@2 = {some, {ok, X}},
                    try_await4_loop(Selector, T1@2, T2, T3, T4, Timer);

                {m4_from_subject2, X@1} ->
                    T2@2 = {some, {ok, X@1}},
                    try_await4_loop(Selector, T1, T2@2, T3, T4, Timer);

                {m4_from_subject3, X@2} ->
                    T3@2 = {some, {ok, X@2}},
                    try_await4_loop(Selector, T1, T2, T3@2, T4, Timer);

                {m4_from_subject4, X@3} ->
                    T4@2 = {some, {ok, X@3}},
                    try_await4_loop(Selector, T1, T2, T3, T4@2, Timer);

                m4_timeout ->
                    {gleam@option:unwrap(T1, {error, timeout}),
                        gleam@option:unwrap(T2, {error, timeout}),
                        gleam@option:unwrap(T3, {error, timeout}),
                        gleam@option:unwrap(T4, {error, timeout})}
            end
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 284).
-spec try_await4(task(HQF), task(HQH), task(HQJ), task(HQL), integer()) -> {{ok,
            HQF} |
        {error, await_error()},
    {ok, HQH} | {error, await_error()},
    {ok, HQJ} | {error, await_error()},
    {ok, HQL} | {error, await_error()}}.
try_await4(Task1, Task2, Task3, Task4, Timeout) ->
    assert_owner(Task1),
    assert_owner(Task2),
    assert_owner(Task3),
    Timeout_subject = gleam@erlang@process:new_subject(),
    Timer = gleam@erlang@process:send_after(
        Timeout_subject,
        Timeout,
        m4_timeout
    ),
    _pipe = gleam_erlang_ffi:new_selector(),
    _pipe@1 = gleam@erlang@process:selecting(
        _pipe,
        erlang:element(4, Task1),
        fun(Field@0) -> {m4_from_subject1, Field@0} end
    ),
    _pipe@2 = gleam@erlang@process:selecting(
        _pipe@1,
        erlang:element(4, Task2),
        fun(Field@0) -> {m4_from_subject2, Field@0} end
    ),
    _pipe@3 = gleam@erlang@process:selecting(
        _pipe@2,
        erlang:element(4, Task3),
        fun(Field@0) -> {m4_from_subject3, Field@0} end
    ),
    _pipe@4 = gleam@erlang@process:selecting(
        _pipe@3,
        erlang:element(4, Task4),
        fun(Field@0) -> {m4_from_subject4, Field@0} end
    ),
    _pipe@5 = gleam@erlang@process:selecting(
        _pipe@4,
        Timeout_subject,
        fun gleam@function:identity/1
    ),
    try_await4_loop(_pipe@5, none, none, none, none, Timer).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 441).
-spec dict_to_list_loop(
    gleam@dict:dict(integer(), HSS),
    HSS,
    integer(),
    list(HSS)
) -> list(HSS).
dict_to_list_loop(Dict, Default, Index, List) ->
    case Index < 0 of
        true ->
            List;

        false ->
            Value@1 = case gleam_stdlib:map_get(Dict, Index) of
                {error, _} ->
                    Default;

                {ok, Value} ->
                    Value
            end,
            dict_to_list_loop(Dict, Default, Index - 1, [Value@1 | List])
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 437).
-spec dict_to_list(gleam@dict:dict(integer(), HSO), integer(), HSO) -> list(HSO).
dict_to_list(Dict, Sized, Default) ->
    dict_to_list_loop(Dict, Default, Sized - 1, []).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 401).
-spec try_await_all_loop(
    gleam@dict:dict(integer(), {ok, HSE} | {error, await_error()}),
    integer(),
    gleam@erlang@process:timer(),
    gleam@erlang@process:selector(message(HSE))
) -> list({ok, HSE} | {error, await_error()}).
try_await_all_loop(Values, Tasks_count, Timer, Selector) ->
    case maps:size(Values) =:= Tasks_count of
        true ->
            gleam@erlang@process:cancel_timer(Timer),
            dict_to_list(Values, Tasks_count, {error, timeout});

        false ->
            case gleam_erlang_ffi:select(Selector) of
                message_timeout ->
                    dict_to_list(Values, Tasks_count, {error, timeout});

                {message, Index, Value} ->
                    Values@1 = gleam@dict:insert(Values, Index, {ok, Value}),
                    try_await_all_loop(Values@1, Tasks_count, Timer, Selector)
            end
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/task.gleam", 374).
-spec try_await_all(list(task(HRY)), integer()) -> list({ok, HRY} |
    {error, await_error()}).
try_await_all(Tasks, Timeout) ->
    {Selector@2, Tasks_count@1} = begin
        Acc = {gleam_erlang_ffi:new_selector(), 0},
        gleam@list:index_fold(
            Tasks,
            Acc,
            fun(_use0, Task, Index) ->
                {Selector, Tasks_count} = _use0,
                assert_owner(Task),
                Selector@1 = gleam@erlang@process:selecting(
                    Selector,
                    erlang:element(4, Task),
                    fun(_capture) -> {message, Index, _capture} end
                ),
                {Selector@1, Tasks_count + 1}
            end
        )
    end,
    Timeout_subject = gleam@erlang@process:new_subject(),
    Timer = gleam@erlang@process:send_after(
        Timeout_subject,
        Timeout,
        message_timeout
    ),
    Selector@3 = gleam@erlang@process:selecting(
        Selector@2,
        Timeout_subject,
        fun gleam@function:identity/1
    ),
    try_await_all_loop(maps:new(), Tasks_count@1, Timer, Selector@3).
