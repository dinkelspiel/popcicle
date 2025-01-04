-module(gleam@erlang@process).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([self/0, start/2, new_subject/0, subject_owner/1, send/2, 'receive'/2, receive_forever/1, new_selector/0, select/2, select_forever/1, map_selector/2, merge_selector/2, flush_messages/0, selecting_trapped_exits/2, selecting/3, selecting_record2/3, selecting_record3/3, selecting_record4/3, selecting_record5/3, selecting_record6/3, selecting_record7/3, selecting_record8/3, selecting_anything/2, deselecting/2, sleep/1, sleep_forever/0, is_alive/1, monitor_process/1, selecting_process_down/3, demonitor_process/1, deselecting_process_down/2, try_call/3, call/3, try_call_forever/2, call_forever/2, link/1, unlink/1, send_after/3, cancel_timer/1, kill/1, send_exit/1, send_abnormal_exit/2, trap_exits/1, register/2, unregister/1, named/1, pid_from_dynamic/1]).
-export_type([pid_/0, subject/1, do_not_leak/0, selector/1, exit_message/0, exit_reason/0, anything_selector_tag/0, process_monitor_flag/0, process_monitor/0, process_down/0, call_error/1, timer/0, cancelled/0, kill_flag/0]).

-type pid_() :: any().

-opaque subject(FOC) :: {subject, pid_(), gleam@erlang:reference_()} |
    {gleam_phantom, FOC}.

-type do_not_leak() :: any().

-type selector(FOD) :: any() | {gleam_phantom, FOD}.

-type exit_message() :: {exit_message, pid_(), exit_reason()}.

-type exit_reason() :: normal | killed | {abnormal, binary()}.

-type anything_selector_tag() :: anything.

-type process_monitor_flag() :: process.

-opaque process_monitor() :: {process_monitor, gleam@erlang:reference_()}.

-type process_down() :: {process_down, pid_(), gleam@dynamic:dynamic_()}.

-type call_error(FOE) :: {callee_down, gleam@dynamic:dynamic_()} |
    call_timeout |
    {gleam_phantom, FOE}.

-type timer() :: any().

-type cancelled() :: timer_not_found | {cancelled, integer()}.

-type kill_flag() :: kill.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 14).
-spec self() -> pid_().
self() ->
    erlang:self().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 28).
-spec start(fun(() -> any()), boolean()) -> pid_().
start(Implementation, Link) ->
    case Link of
        true ->
            erlang:spawn_link(Implementation);

        false ->
            erlang:spawn(Implementation)
    end.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 70).
-spec new_subject() -> subject(any()).
new_subject() ->
    {subject, erlang:self(), erlang:make_ref()}.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 77).
-spec subject_owner(subject(any())) -> pid_().
subject_owner(Subject) ->
    erlang:element(2, Subject).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 110).
-spec send(subject(FON), FON) -> nil.
send(Subject, Message) ->
    erlang:send(
        erlang:element(2, Subject),
        {erlang:element(3, Subject), Message}
    ),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 131).
-spec 'receive'(subject(FOP), integer()) -> {ok, FOP} | {error, nil}.
'receive'(Subject, Timeout) ->
    gleam_erlang_ffi:'receive'(Subject, Timeout).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 140).
-spec receive_forever(subject(FOT)) -> FOT.
receive_forever(Subject) ->
    gleam_erlang_ffi:'receive'(Subject).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 169).
-spec new_selector() -> selector(any()).
new_selector() ->
    gleam_erlang_ffi:new_selector().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 189).
-spec select(selector(FOX), integer()) -> {ok, FOX} | {error, nil}.
select(From, Within) ->
    gleam_erlang_ffi:select(From, Within).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 198).
-spec select_forever(selector(FPB)) -> FPB.
select_forever(From) ->
    gleam_erlang_ffi:select(From).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 207).
-spec map_selector(selector(FPD), fun((FPD) -> FPF)) -> selector(FPF).
map_selector(A, B) ->
    gleam_erlang_ffi:map_selector(A, B).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 216).
-spec merge_selector(selector(FPH), selector(FPH)) -> selector(FPH).
merge_selector(A, B) ->
    gleam_erlang_ffi:merge_selector(A, B).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 260).
-spec flush_messages() -> nil.
flush_messages() ->
    gleam_erlang_ffi:flush_messages().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 232).
-spec selecting_trapped_exits(selector(FPL), fun((exit_message()) -> FPL)) -> selector(FPL).
selecting_trapped_exits(Selector, Handler) ->
    Tag = erlang:binary_to_atom(<<"EXIT"/utf8>>),
    Handler@1 = fun(Message) ->
        Reason = erlang:element(3, Message),
        Normal = gleam@dynamic:from(normal),
        Killed = gleam@dynamic:from(killed),
        Reason@2 = case gleam@dynamic:string(Reason) of
            _ when Reason =:= Normal ->
                normal;

            _ when Reason =:= Killed ->
                killed;

            {ok, Reason@1} ->
                {abnormal, Reason@1};

            {error, _} ->
                {abnormal, gleam@string:inspect(Reason)}
        end,
        Handler({exit_message, erlang:element(2, Message), Reason@2})
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 3}, Handler@1).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 273).
-spec selecting(selector(FPO), subject(FPQ), fun((FPQ) -> FPO)) -> selector(FPO).
selecting(Selector, Subject, Transform) ->
    Handler = fun(Message) -> Transform(erlang:element(2, Message)) end,
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        {erlang:element(3, Subject), 2},
        Handler
    ).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 299).
-spec selecting_record2(
    selector(FPY),
    any(),
    fun((gleam@dynamic:dynamic_()) -> FPY)
) -> selector(FPY).
selecting_record2(Selector, Tag, Transform) ->
    Handler = fun(Message) -> Transform(erlang:element(2, Message)) end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 2}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 315).
-spec selecting_record3(
    selector(FQC),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FQC)
) -> selector(FQC).
selecting_record3(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(erlang:element(2, Message), erlang:element(3, Message))
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 3}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 333).
-spec selecting_record4(
    selector(FQG),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FQG)
) -> selector(FQG).
selecting_record4(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 4}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 351).
-spec selecting_record5(
    selector(FQK),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FQK)
) -> selector(FQK).
selecting_record5(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 5}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 369).
-spec selecting_record6(
    selector(FQO),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FQO)
) -> selector(FQO).
selecting_record6(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 6}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 387).
-spec selecting_record7(
    selector(FQS),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FQS)
) -> selector(FQS).
selecting_record7(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message),
            erlang:element(7, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 7}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 408).
-spec selecting_record8(
    selector(FQW),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FQW)
) -> selector(FQW).
selecting_record8(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message),
            erlang:element(7, Message),
            erlang:element(8, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 8}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 458).
-spec selecting_anything(selector(FRA), fun((gleam@dynamic:dynamic_()) -> FRA)) -> selector(FRA).
selecting_anything(Selector, Handler) ->
    gleam_erlang_ffi:insert_selector_handler(Selector, anything, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 285).
-spec deselecting(selector(FPT), subject(any())) -> selector(FPT).
deselecting(Selector, Subject) ->
    gleam_erlang_ffi:remove_selector_handler(
        Selector,
        {erlang:element(3, Subject), 2}
    ).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 482).
-spec sleep(integer()) -> nil.
sleep(A) ->
    gleam_erlang_ffi:sleep(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 489).
-spec sleep_forever() -> nil.
sleep_forever() ->
    gleam_erlang_ffi:sleep_forever().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 498).
-spec is_alive(pid_()) -> boolean().
is_alive(A) ->
    erlang:is_process_alive(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 529).
-spec monitor_process(pid_()) -> process_monitor().
monitor_process(Pid) ->
    _pipe = process,
    _pipe@1 = erlang:monitor(_pipe, Pid),
    {process_monitor, _pipe@1}.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 540).
-spec selecting_process_down(
    selector(FRM),
    process_monitor(),
    fun((process_down()) -> FRM)
) -> selector(FRM).
selecting_process_down(Selector, Monitor, Mapping) ->
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        erlang:element(2, Monitor),
        Mapping
    ).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 555).
-spec demonitor_process(process_monitor()) -> nil.
demonitor_process(Monitor) ->
    gleam_erlang_ffi:demonitor(Monitor).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 574).
-spec deselecting_process_down(selector(FRP), process_monitor()) -> selector(FRP).
deselecting_process_down(Selector, Monitor) ->
    gleam_erlang_ffi:remove_selector_handler(
        Selector,
        erlang:element(2, Monitor)
    ).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 589).
-spec try_call(subject(FRS), fun((subject(FRU)) -> FRS), integer()) -> {ok, FRU} |
    {error, call_error(FRU)}.
try_call(Subject, Make_request, Timeout) ->
    Reply_subject = new_subject(),
    Monitor = monitor_process(subject_owner(Subject)),
    send(Subject, Make_request(Reply_subject)),
    Result = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = selecting(
            _pipe,
            Reply_subject,
            fun(Field@0) -> {ok, Field@0} end
        ),
        _pipe@2 = selecting_process_down(
            _pipe@1,
            Monitor,
            fun(Down) -> {error, {callee_down, erlang:element(3, Down)}} end
        ),
        gleam_erlang_ffi:select(_pipe@2, Timeout)
    end,
    gleam_erlang_ffi:demonitor(Monitor),
    case Result of
        {error, nil} ->
            {error, call_timeout};

        {ok, Res} ->
            Res
    end.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 630).
-spec call(subject(FRZ), fun((subject(FSB)) -> FRZ), integer()) -> FSB.
call(Subject, Make_request, Timeout) ->
    _assert_subject = try_call(Subject, Make_request, Timeout),
    {ok, Resp} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam/erlang/process"/utf8>>,
                        function => <<"call"/utf8>>,
                        line => 635})
    end,
    Resp.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 659).
-spec try_call_forever(subject(FSH), fun((subject(FSJ)) -> FSH)) -> {ok, FSJ} |
    {error, call_error(any())}.
try_call_forever(Subject, Make_request) ->
    Reply_subject = new_subject(),
    Monitor = monitor_process(subject_owner(Subject)),
    send(Subject, Make_request(Reply_subject)),
    Result = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = selecting(
            _pipe,
            Reply_subject,
            fun(Field@0) -> {ok, Field@0} end
        ),
        _pipe@2 = selecting_process_down(
            _pipe@1,
            Monitor,
            fun(Down) -> {error, {callee_down, erlang:element(3, Down)}} end
        ),
        gleam_erlang_ffi:select(_pipe@2)
    end,
    gleam_erlang_ffi:demonitor(Monitor),
    Result.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 646).
-spec call_forever(subject(FSD), fun((subject(FSF)) -> FSD)) -> FSF.
call_forever(Subject, Make_request) ->
    _assert_subject = try_call_forever(Subject, Make_request),
    {ok, Response} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam/erlang/process"/utf8>>,
                        function => <<"call_forever"/utf8>>,
                        line => 650})
    end,
    Response.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 697).
-spec link(pid_()) -> boolean().
link(Pid) ->
    gleam_erlang_ffi:link(Pid).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 704).
-spec unlink(pid_()) -> nil.
unlink(Pid) ->
    erlang:unlink(Pid),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 716).
-spec send_after(subject(FSQ), integer(), FSQ) -> timer().
send_after(Subject, Delay, Message) ->
    erlang:send_after(
        Delay,
        erlang:element(2, Subject),
        {erlang:element(3, Subject), Message}
    ).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 740).
-spec cancel_timer(timer()) -> cancelled().
cancel_timer(Timer) ->
    case gleam@dynamic:int(erlang:cancel_timer(Timer)) of
        {ok, I} ->
            {cancelled, I};

        {error, _} ->
            timer_not_found
    end.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 764).
-spec kill(pid_()) -> nil.
kill(Pid) ->
    erlang:exit(Pid, kill),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 780).
-spec send_exit(pid_()) -> nil.
send_exit(Pid) ->
    erlang:exit(Pid, normal),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 792).
-spec send_abnormal_exit(pid_(), binary()) -> nil.
send_abnormal_exit(Pid, Reason) ->
    erlang:exit(Pid, {abnormal, Reason}),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 808).
-spec trap_exits(boolean()) -> nil.
trap_exits(A) ->
    gleam_erlang_ffi:trap_exits(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 820).
-spec register(pid_(), gleam@erlang@atom:atom_()) -> {ok, nil} | {error, nil}.
register(Pid, Name) ->
    gleam_erlang_ffi:register_process(Pid, Name).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 831).
-spec unregister(gleam@erlang@atom:atom_()) -> {ok, nil} | {error, nil}.
unregister(Name) ->
    gleam_erlang_ffi:unregister_process(Name).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 836).
-spec named(gleam@erlang@atom:atom_()) -> {ok, pid_()} | {error, nil}.
named(Name) ->
    gleam_erlang_ffi:process_named(Name).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 854).
-spec pid_from_dynamic(gleam@dynamic:dynamic_()) -> {ok, pid_()} |
    {error, list(gleam@dynamic:decode_error())}.
pid_from_dynamic(From) ->
    gleam_erlang_ffi:pid_from_dynamic(From).
