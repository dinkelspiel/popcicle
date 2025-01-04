-module(lustre@attribute).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([attribute/2, property/2, on/2, map/2, style/1, class/1, none/0, classes/1, id/1, role/1, title/1, type_/1, value/1, checked/1, placeholder/1, selected/1, accept/1, accept_charset/1, msg/1, autocomplete/1, autofocus/1, disabled/1, name/1, pattern/1, readonly/1, required/1, for/1, max/1, min/1, step/1, cols/1, rows/1, wrap/1, href/1, target/1, download/1, rel/1, src/1, height/1, width/1, alt/1, content/1, autoplay/1, controls/1, loop/1, action/1, enctype/1, method/1, novalidate/1, form_action/1, form_enctype/1, form_method/1, form_novalidate/1, form_target/1, open/1, charset/1, http_equiv/1]).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 27).
-spec attribute(binary(), binary()) -> lustre@internals@vdom:attribute(any()).
attribute(Name, Value) ->
    {attribute, Name, gleam_stdlib:identity(Value), false}.

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 39).
-spec property(binary(), any()) -> lustre@internals@vdom:attribute(any()).
property(Name, Value) ->
    {attribute, Name, gleam_stdlib:identity(Value), true}.

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 44).
-spec on(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, OYU} |
        {error, list(gleam@dynamic:decode_error())})
) -> lustre@internals@vdom:attribute(OYU).
on(Name, Handler) ->
    {event, <<"on"/utf8, Name/binary>>, Handler}.

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 63).
-spec map(lustre@internals@vdom:attribute(OYZ), fun((OYZ) -> OZB)) -> lustre@internals@vdom:attribute(OZB).
map(Attr, F) ->
    case Attr of
        {attribute, Name, Value, As_property} ->
            {attribute, Name, Value, As_property};

        {event, On, Handler} ->
            {event, On, fun(E) -> gleam@result:map(Handler(E), F) end}
    end.

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 78).
-spec style(list({binary(), binary()})) -> lustre@internals@vdom:attribute(any()).
style(Properties) ->
    attribute(
        <<"style"/utf8>>,
        (gleam@list:fold(
            Properties,
            <<""/utf8>>,
            fun(Styles, _use1) ->
                {Name, Value} = _use1,
                <<<<<<<<Styles/binary, Name/binary>>/binary, ":"/utf8>>/binary,
                        Value/binary>>/binary,
                    ";"/utf8>>
            end
        ))
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 90).
-spec class(binary()) -> lustre@internals@vdom:attribute(any()).
class(Name) ->
    attribute(<<"class"/utf8>>, Name).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 52).
-spec none() -> lustre@internals@vdom:attribute(any()).
none() ->
    class(<<""/utf8>>).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 95).
-spec classes(list({binary(), boolean()})) -> lustre@internals@vdom:attribute(any()).
classes(Names) ->
    attribute(
        <<"class"/utf8>>,
        begin
            _pipe = Names,
            _pipe@1 = gleam@list:filter_map(
                _pipe,
                fun(Class) -> case erlang:element(2, Class) of
                        true ->
                            {ok, erlang:element(1, Class)};

                        false ->
                            {error, nil}
                    end end
            ),
            gleam@string:join(_pipe@1, <<" "/utf8>>)
        end
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 110).
-spec id(binary()) -> lustre@internals@vdom:attribute(any()).
id(Name) ->
    attribute(<<"id"/utf8>>, Name).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 115).
-spec role(binary()) -> lustre@internals@vdom:attribute(any()).
role(Name) ->
    attribute(<<"role"/utf8>>, Name).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 120).
-spec title(binary()) -> lustre@internals@vdom:attribute(any()).
title(Name) ->
    attribute(<<"title"/utf8>>, Name).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 127).
-spec type_(binary()) -> lustre@internals@vdom:attribute(any()).
type_(Name) ->
    attribute(<<"type"/utf8>>, Name).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 132).
-spec value(binary()) -> lustre@internals@vdom:attribute(any()).
value(Val) ->
    attribute(<<"value"/utf8>>, Val).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 137).
-spec checked(boolean()) -> lustre@internals@vdom:attribute(any()).
checked(Is_checked) ->
    property(<<"checked"/utf8>>, Is_checked).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 142).
-spec placeholder(binary()) -> lustre@internals@vdom:attribute(any()).
placeholder(Text) ->
    attribute(<<"placeholder"/utf8>>, Text).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 147).
-spec selected(boolean()) -> lustre@internals@vdom:attribute(any()).
selected(Is_selected) ->
    property(<<"selected"/utf8>>, Is_selected).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 154).
-spec accept(list(binary())) -> lustre@internals@vdom:attribute(any()).
accept(Types) ->
    attribute(<<"accept"/utf8>>, gleam@string:join(Types, <<","/utf8>>)).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 159).
-spec accept_charset(list(binary())) -> lustre@internals@vdom:attribute(any()).
accept_charset(Types) ->
    attribute(<<"acceptCharset"/utf8>>, gleam@string:join(Types, <<" "/utf8>>)).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 164).
-spec msg(binary()) -> lustre@internals@vdom:attribute(any()).
msg(Uri) ->
    attribute(<<"msg"/utf8>>, Uri).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 169).
-spec autocomplete(binary()) -> lustre@internals@vdom:attribute(any()).
autocomplete(Name) ->
    attribute(<<"autocomplete"/utf8>>, Name).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 174).
-spec autofocus(boolean()) -> lustre@internals@vdom:attribute(any()).
autofocus(Should_autofocus) ->
    property(<<"autofocus"/utf8>>, Should_autofocus).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 179).
-spec disabled(boolean()) -> lustre@internals@vdom:attribute(any()).
disabled(Is_disabled) ->
    property(<<"disabled"/utf8>>, Is_disabled).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 184).
-spec name(binary()) -> lustre@internals@vdom:attribute(any()).
name(Name) ->
    attribute(<<"name"/utf8>>, Name).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 189).
-spec pattern(binary()) -> lustre@internals@vdom:attribute(any()).
pattern(Regex) ->
    attribute(<<"pattern"/utf8>>, Regex).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 194).
-spec readonly(boolean()) -> lustre@internals@vdom:attribute(any()).
readonly(Is_readonly) ->
    property(<<"readOnly"/utf8>>, Is_readonly).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 199).
-spec required(boolean()) -> lustre@internals@vdom:attribute(any()).
required(Is_required) ->
    property(<<"required"/utf8>>, Is_required).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 204).
-spec for(binary()) -> lustre@internals@vdom:attribute(any()).
for(Id) ->
    attribute(<<"for"/utf8>>, Id).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 211).
-spec max(binary()) -> lustre@internals@vdom:attribute(any()).
max(Val) ->
    attribute(<<"max"/utf8>>, Val).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 216).
-spec min(binary()) -> lustre@internals@vdom:attribute(any()).
min(Val) ->
    attribute(<<"min"/utf8>>, Val).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 221).
-spec step(binary()) -> lustre@internals@vdom:attribute(any()).
step(Val) ->
    attribute(<<"step"/utf8>>, Val).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 228).
-spec cols(integer()) -> lustre@internals@vdom:attribute(any()).
cols(Val) ->
    attribute(<<"cols"/utf8>>, gleam@int:to_string(Val)).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 233).
-spec rows(integer()) -> lustre@internals@vdom:attribute(any()).
rows(Val) ->
    attribute(<<"rows"/utf8>>, gleam@int:to_string(Val)).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 238).
-spec wrap(binary()) -> lustre@internals@vdom:attribute(any()).
wrap(Mode) ->
    attribute(<<"wrap"/utf8>>, Mode).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 245).
-spec href(binary()) -> lustre@internals@vdom:attribute(any()).
href(Uri) ->
    attribute(<<"href"/utf8>>, Uri).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 250).
-spec target(binary()) -> lustre@internals@vdom:attribute(any()).
target(Target) ->
    attribute(<<"target"/utf8>>, Target).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 255).
-spec download(binary()) -> lustre@internals@vdom:attribute(any()).
download(Filename) ->
    attribute(<<"download"/utf8>>, Filename).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 260).
-spec rel(binary()) -> lustre@internals@vdom:attribute(any()).
rel(Relationship) ->
    attribute(<<"rel"/utf8>>, Relationship).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 267).
-spec src(binary()) -> lustre@internals@vdom:attribute(any()).
src(Uri) ->
    attribute(<<"src"/utf8>>, Uri).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 278).
-spec height(integer()) -> lustre@internals@vdom:attribute(any()).
height(Val) ->
    property(<<"height"/utf8>>, Val).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 289).
-spec width(integer()) -> lustre@internals@vdom:attribute(any()).
width(Val) ->
    property(<<"width"/utf8>>, Val).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 294).
-spec alt(binary()) -> lustre@internals@vdom:attribute(any()).
alt(Text) ->
    attribute(<<"alt"/utf8>>, Text).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 299).
-spec content(binary()) -> lustre@internals@vdom:attribute(any()).
content(Text) ->
    attribute(<<"content"/utf8>>, Text).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 306).
-spec autoplay(boolean()) -> lustre@internals@vdom:attribute(any()).
autoplay(Should_autoplay) ->
    property(<<"autoplay"/utf8>>, Should_autoplay).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 311).
-spec controls(boolean()) -> lustre@internals@vdom:attribute(any()).
controls(Visible) ->
    property(<<"controls"/utf8>>, Visible).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 316).
-spec loop(boolean()) -> lustre@internals@vdom:attribute(any()).
loop(Should_loop) ->
    property(<<"loop"/utf8>>, Should_loop).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 323).
-spec action(binary()) -> lustre@internals@vdom:attribute(any()).
action(Url) ->
    attribute(<<"action"/utf8>>, Url).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 328).
-spec enctype(binary()) -> lustre@internals@vdom:attribute(any()).
enctype(Value) ->
    attribute(<<"enctype"/utf8>>, Value).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 333).
-spec method(binary()) -> lustre@internals@vdom:attribute(any()).
method(Method) ->
    attribute(<<"method"/utf8>>, Method).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 338).
-spec novalidate(boolean()) -> lustre@internals@vdom:attribute(any()).
novalidate(Value) ->
    property(<<"novalidate"/utf8>>, Value).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 343).
-spec form_action(binary()) -> lustre@internals@vdom:attribute(any()).
form_action(Action) ->
    attribute(<<"formaction"/utf8>>, Action).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 348).
-spec form_enctype(binary()) -> lustre@internals@vdom:attribute(any()).
form_enctype(Value) ->
    attribute(<<"formenctype"/utf8>>, Value).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 353).
-spec form_method(binary()) -> lustre@internals@vdom:attribute(any()).
form_method(Method) ->
    attribute(<<"formmethod"/utf8>>, Method).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 358).
-spec form_novalidate(boolean()) -> lustre@internals@vdom:attribute(any()).
form_novalidate(Value) ->
    property(<<"formnovalidate"/utf8>>, Value).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 363).
-spec form_target(binary()) -> lustre@internals@vdom:attribute(any()).
form_target(Target) ->
    attribute(<<"formtarget"/utf8>>, Target).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 370).
-spec open(boolean()) -> lustre@internals@vdom:attribute(any()).
open(Is_open) ->
    property(<<"open"/utf8>>, Is_open).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 377).
-spec charset(binary()) -> lustre@internals@vdom:attribute(any()).
charset(Name) ->
    attribute(<<"charset"/utf8>>, Name).

-file("/home/runner/work/lustre/lustre/src/lustre/attribute.gleam", 382).
-spec http_equiv(binary()) -> lustre@internals@vdom:attribute(any()).
http_equiv(Name) ->
    attribute(<<"http-equiv"/utf8>>, Name).
