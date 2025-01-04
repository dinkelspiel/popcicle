-module(gleam@otp@static_supervisor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/1, restart_tolerance/3, auto_shutdown/2, add/2, worker_child/2, supervisor_child/2, significant/2, timeout/2, restart/2, start_link/1, init/1]).
-export_type([strategy/0, auto_shutdown/0, builder/0, restart/0, child_type/0, child_builder/0]).

-type strategy() :: one_for_one | one_for_all | rest_for_one.

-type auto_shutdown() :: never | any_significant | all_significant.

-opaque builder() :: {builder,
        strategy(),
        integer(),
        integer(),
        auto_shutdown(),
        list(child_builder())}.

-type restart() :: permanent | transient | temporary.

-type child_type() :: {worker, integer()} | supervisor.

-opaque child_builder() :: {child_builder,
        binary(),
        fun(() -> {ok, gleam@erlang@process:pid_()} |
            {error, gleam@dynamic:dynamic_()}),
        restart(),
        boolean(),
        child_type()}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 74).
-spec new(strategy()) -> builder().
new(Strategy) ->
    {builder, Strategy, 2, 5, never, []}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 94).
-spec restart_tolerance(builder(), integer(), integer()) -> builder().
restart_tolerance(Builder, Intensity, Period) ->
    _record = Builder,
    {builder,
        erlang:element(2, _record),
        Intensity,
        Period,
        erlang:element(5, _record),
        erlang:element(6, _record)}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 104).
-spec auto_shutdown(builder(), auto_shutdown()) -> builder().
auto_shutdown(Builder, Value) ->
    _record = Builder,
    {builder,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        Value,
        erlang:element(6, _record)}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 184).
-spec add(builder(), child_builder()) -> builder().
add(Builder, Child) ->
    _record = Builder,
    {builder,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        [Child | erlang:element(6, Builder)]}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 197).
-spec worker_child(
    binary(),
    fun(() -> {ok, gleam@erlang@process:pid_()} | {error, any()})
) -> child_builder().
worker_child(Id, Starter) ->
    {child_builder, Id, fun() -> _pipe = Starter(),
            gleam@result:map_error(_pipe, fun gleam_stdlib:identity/1) end, permanent, false, {worker,
            5000}}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 219).
-spec supervisor_child(
    binary(),
    fun(() -> {ok, gleam@erlang@process:pid_()} | {error, any()})
) -> child_builder().
supervisor_child(Id, Starter) ->
    {child_builder, Id, fun() -> _pipe = Starter(),
            gleam@result:map_error(_pipe, fun gleam_stdlib:identity/1) end, permanent, false, supervisor}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 241).
-spec significant(child_builder(), boolean()) -> child_builder().
significant(Child, Significant) ->
    _record = Child,
    {child_builder,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        Significant,
        erlang:element(6, _record)}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 252).
-spec timeout(child_builder(), integer()) -> child_builder().
timeout(Child, Ms) ->
    case erlang:element(6, Child) of
        {worker, _} ->
            _record = Child,
            {child_builder,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                {worker, Ms}};

        _ ->
            Child
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 263).
-spec restart(child_builder(), restart()) -> child_builder().
restart(Child, Restart) ->
    _record = Child,
    {child_builder,
        erlang:element(2, _record),
        erlang:element(3, _record),
        Restart,
        erlang:element(5, _record),
        erlang:element(6, _record)}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 294).
-spec property(
    gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()),
    binary(),
    any()
) -> gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()).
property(Dict, Key, Value) ->
    gleam@dict:insert(
        Dict,
        erlang:binary_to_atom(Key),
        gleam_stdlib:identity(Value)
    ).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 267).
-spec convert_child(child_builder()) -> gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()).
convert_child(Child) ->
    Mfa = {erlang:binary_to_atom(<<"erlang"/utf8>>),
        erlang:binary_to_atom(<<"apply"/utf8>>),
        [gleam_stdlib:identity(erlang:element(3, Child)),
            gleam_stdlib:identity([])]},
    {Type_, Shutdown} = case erlang:element(6, Child) of
        supervisor ->
            {erlang:binary_to_atom(<<"supervisor"/utf8>>),
                gleam_stdlib:identity(
                    erlang:binary_to_atom(<<"infinity"/utf8>>)
                )};

        {worker, Timeout} ->
            {erlang:binary_to_atom(<<"worker"/utf8>>),
                gleam_stdlib:identity(Timeout)}
    end,
    _pipe = maps:new(),
    _pipe@1 = property(_pipe, <<"id"/utf8>>, erlang:element(2, Child)),
    _pipe@2 = property(_pipe@1, <<"start"/utf8>>, Mfa),
    _pipe@3 = property(_pipe@2, <<"restart"/utf8>>, erlang:element(4, Child)),
    _pipe@4 = property(
        _pipe@3,
        <<"significant"/utf8>>,
        erlang:element(5, Child)
    ),
    _pipe@5 = property(_pipe@4, <<"type"/utf8>>, Type_),
    property(_pipe@5, <<"shutdown"/utf8>>, Shutdown).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 165).
-spec start_link(builder()) -> {ok, gleam@erlang@process:pid_()} |
    {error, gleam@dynamic:dynamic_()}.
start_link(Builder) ->
    Flags = begin
        _pipe = maps:new(),
        _pipe@1 = property(
            _pipe,
            <<"strategy"/utf8>>,
            erlang:element(2, Builder)
        ),
        _pipe@2 = property(
            _pipe@1,
            <<"intensity"/utf8>>,
            erlang:element(3, Builder)
        ),
        _pipe@3 = property(
            _pipe@2,
            <<"period"/utf8>>,
            erlang:element(4, Builder)
        ),
        property(_pipe@3, <<"auto_shutdown"/utf8>>, erlang:element(5, Builder))
    end,
    Children = begin
        _pipe@4 = erlang:element(6, Builder),
        _pipe@5 = lists:reverse(_pipe@4),
        gleam@list:map(_pipe@5, fun convert_child/1)
    end,
    gleam_otp_external:static_supervisor_start_link({Flags, Children}).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 304).
-spec init(gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
    {error, any()}.
init(Start_data) ->
    {ok, Start_data}.
