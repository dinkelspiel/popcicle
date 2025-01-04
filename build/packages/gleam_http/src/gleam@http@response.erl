-module(gleam@http@response).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/1, get_header/2, set_header/3, prepend_header/3, set_body/2, try_map/2, map/2, redirect/1, get_cookies/1, set_cookie/4, expire_cookie/3]).
-export_type([response/1]).

-type response(GXV) :: {response, integer(), list({binary(), binary()}), GXV}.

-file("/Users/louis/src/gleam/http/src/gleam/http/response.gleam", 40).
-spec new(integer()) -> response(binary()).
new(Status) ->
    {response, Status, [], <<""/utf8>>}.

-file("/Users/louis/src/gleam/http/src/gleam/http/response.gleam", 48).
-spec get_header(response(any()), binary()) -> {ok, binary()} | {error, nil}.
get_header(Response, Key) ->
    gleam@list:key_find(erlang:element(3, Response), string:lowercase(Key)).

-file("/Users/louis/src/gleam/http/src/gleam/http/response.gleam", 59).
-spec set_header(response(GYK), binary(), binary()) -> response(GYK).
set_header(Response, Key, Value) ->
    Headers = gleam@list:key_set(
        erlang:element(3, Response),
        string:lowercase(Key),
        Value
    ),
    _record = Response,
    {response, erlang:element(2, _record), Headers, erlang:element(4, _record)}.

-file("/Users/louis/src/gleam/http/src/gleam/http/response.gleam", 76).
-spec prepend_header(response(GYN), binary(), binary()) -> response(GYN).
prepend_header(Response, Key, Value) ->
    Headers = [{string:lowercase(Key), Value} | erlang:element(3, Response)],
    _record = Response,
    {response, erlang:element(2, _record), Headers, erlang:element(4, _record)}.

-file("/Users/louis/src/gleam/http/src/gleam/http/response.gleam", 87).
-spec set_body(response(any()), GYS) -> response(GYS).
set_body(Response, Body) ->
    {response, Status, Headers, _} = Response,
    {response, Status, Headers, Body}.

-file("/Users/louis/src/gleam/http/src/gleam/http/response.gleam", 27).
-spec try_map(response(GXW), fun((GXW) -> {ok, GXY} | {error, GXZ})) -> {ok,
        response(GXY)} |
    {error, GXZ}.
try_map(Response, Transform) ->
    gleam@result:then(
        Transform(erlang:element(4, Response)),
        fun(Body) -> {ok, set_body(Response, Body)} end
    ).

-file("/Users/louis/src/gleam/http/src/gleam/http/response.gleam", 97).
-spec map(response(GYU), fun((GYU) -> GYW)) -> response(GYW).
map(Response, Transform) ->
    _pipe = erlang:element(4, Response),
    _pipe@1 = Transform(_pipe),
    set_body(Response, _pipe@1).

-file("/Users/louis/src/gleam/http/src/gleam/http/response.gleam", 108).
-spec redirect(binary()) -> response(binary()).
redirect(Uri) ->
    {response,
        303,
        [{<<"location"/utf8>>, Uri}],
        gleam@string:append(<<"You are being redirected to "/utf8>>, Uri)}.

-file("/Users/louis/src/gleam/http/src/gleam/http/response.gleam", 120).
-spec get_cookies(response(any())) -> list({binary(), binary()}).
get_cookies(Resp) ->
    {response, _, Headers, _} = Resp,
    _pipe = Headers,
    _pipe@1 = gleam@list:filter_map(
        _pipe,
        fun(Header) ->
            {Name, Value} = Header,
            case Name of
                <<"set-cookie"/utf8>> ->
                    {ok, gleam@http@cookie:parse(Value)};

                _ ->
                    {error, nil}
            end
        end
    ),
    gleam@list:flatten(_pipe@1).

-file("/Users/louis/src/gleam/http/src/gleam/http/response.gleam", 135).
-spec set_cookie(
    response(GZB),
    binary(),
    binary(),
    gleam@http@cookie:attributes()
) -> response(GZB).
set_cookie(Response, Name, Value, Attributes) ->
    prepend_header(
        Response,
        <<"set-cookie"/utf8>>,
        gleam@http@cookie:set_header(Name, Value, Attributes)
    ).

-file("/Users/louis/src/gleam/http/src/gleam/http/response.gleam", 151).
-spec expire_cookie(response(GZE), binary(), gleam@http@cookie:attributes()) -> response(GZE).
expire_cookie(Response, Name, Attributes) ->
    Attrs = begin
        _record = Attributes,
        {attributes,
            {some, 0},
            erlang:element(3, _record),
            erlang:element(4, _record),
            erlang:element(5, _record),
            erlang:element(6, _record),
            erlang:element(7, _record)}
    end,
    set_cookie(Response, Name, <<""/utf8>>, Attrs).
