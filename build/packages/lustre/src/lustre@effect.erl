-module(lustre@effect).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([custom/1, from/1, event/2, none/0, batch/1, map/2, perform/5]).
-export_type([effect/1, actions/1]).

-opaque effect(OIG) :: {effect, list(fun((actions(OIG)) -> nil))}.

-type actions(OIH) :: {actions,
        fun((OIH) -> nil),
        fun((binary(), gleam@json:json()) -> nil),
        fun((gleam@erlang@process:selector(OIH)) -> nil),
        gleam@dynamic:dynamic_()}.

-file("/home/runner/work/lustre/lustre/src/lustre/effect.gleam", 126).
-spec custom(
    fun((fun((OIM) -> nil), fun((binary(), gleam@json:json()) -> nil), fun((gleam@erlang@process:selector(OIM)) -> nil), gleam@dynamic:dynamic_()) -> nil)
) -> effect(OIM).
custom(Run) ->
    {effect,
        [fun(Actions) ->
                Run(
                    erlang:element(2, Actions),
                    erlang:element(3, Actions),
                    erlang:element(4, Actions),
                    erlang:element(5, Actions)
                )
            end]}.

-file("/home/runner/work/lustre/lustre/src/lustre/effect.gleam", 105).
-spec from(fun((fun((OII) -> nil)) -> nil)) -> effect(OII).
from(Effect) ->
    custom(fun(Dispatch, _, _, _) -> Effect(Dispatch) end).

-file("/home/runner/work/lustre/lustre/src/lustre/effect.gleam", 117).
-spec event(binary(), gleam@json:json()) -> effect(any()).
event(Name, Data) ->
    custom(fun(_, Emit, _, _) -> Emit(Name, Data) end).

-file("/home/runner/work/lustre/lustre/src/lustre/effect.gleam", 146).
-spec none() -> effect(any()).
none() ->
    {effect, []}.

-file("/home/runner/work/lustre/lustre/src/lustre/effect.gleam", 164).
-spec batch(list(effect(OIR))) -> effect(OIR).
batch(Effects) ->
    {effect,
        (gleam@list:fold(
            Effects,
            [],
            fun(B, _use1) ->
                {effect, A} = _use1,
                lists:append(B, A)
            end
        ))}.

-file("/home/runner/work/lustre/lustre/src/lustre/effect.gleam", 178).
-spec map(effect(OIV), fun((OIV) -> OIX)) -> effect(OIX).
map(Effect, F) ->
    {effect,
        (gleam@list:map(
            erlang:element(2, Effect),
            fun(Eff) ->
                fun(Actions) ->
                    Eff(
                        {actions,
                            fun(Msg) -> (erlang:element(2, Actions))(F(Msg)) end,
                            erlang:element(3, Actions),
                            fun(Selector) ->
                                (erlang:element(4, Actions))(
                                    gleam_erlang_ffi:map_selector(Selector, F)
                                )
                            end,
                            erlang:element(5, Actions)}
                    )
                end
            end
        ))}.

-file("/home/runner/work/lustre/lustre/src/lustre/effect.gleam", 230).
-spec perform(
    effect(OIZ),
    fun((OIZ) -> nil),
    fun((binary(), gleam@json:json()) -> nil),
    fun((gleam@erlang@process:selector(OIZ)) -> nil),
    gleam@dynamic:dynamic_()
) -> nil.
perform(Effect, Dispatch, Emit, Select, Root) ->
    Actions = {actions, Dispatch, Emit, Select, Root},
    gleam@list:each(erlang:element(2, Effect), fun(Eff) -> Eff(Actions) end).
