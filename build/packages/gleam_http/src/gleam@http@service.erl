-module(gleam@http@service).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([map_response_body/2, prepend_response_header/3, method_override/1]).

-file("/Users/louis/src/gleam/http/src/gleam/http/service.gleam", 17).
-spec map_response_body(
    fun((HCK) -> gleam@http@response:response(HBW)),
    fun((HBW) -> HBX)
) -> fun((HCK) -> gleam@http@response:response(HBX)).
map_response_body(Service, Mapper) ->
    fun(Req) -> _pipe = Req,
        _pipe@1 = Service(_pipe),
        gleam@http@response:map(_pipe@1, Mapper) end.

-file("/Users/louis/src/gleam/http/src/gleam/http/service.gleam", 26).
-spec prepend_response_header(
    fun((HCO) -> gleam@http@response:response(HCQ)),
    binary(),
    binary()
) -> fun((HCO) -> gleam@http@response:response(HCQ)).
prepend_response_header(Service, Key, Value) ->
    fun(Req) -> _pipe = Req,
        _pipe@1 = Service(_pipe),
        gleam@http@response:prepend_header(_pipe@1, Key, Value) end.

-file("/Users/louis/src/gleam/http/src/gleam/http/service.gleam", 34).
-spec ensure_post(gleam@http@request:request(HCB)) -> {ok,
        gleam@http@request:request(HCB)} |
    {error, nil}.
ensure_post(Req) ->
    case erlang:element(2, Req) of
        post ->
            {ok, Req};

        _ ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http/service.gleam", 41).
-spec get_override_method(gleam@http@request:request(any())) -> {ok,
        gleam@http:method()} |
    {error, nil}.
get_override_method(Request) ->
    gleam@result:then(
        gleam@http@request:get_query(Request),
        fun(Query_params) ->
            gleam@result:then(
                gleam@list:key_find(Query_params, <<"_method"/utf8>>),
                fun(Method) ->
                    gleam@result:then(
                        gleam@http:parse_method(Method),
                        fun(Method@1) -> case Method@1 of
                                put ->
                                    {ok, Method@1};

                                patch ->
                                    {ok, Method@1};

                                delete ->
                                    {ok, Method@1};

                                _ ->
                                    {error, nil}
                            end end
                    )
                end
            )
        end
    ).

-file("/Users/louis/src/gleam/http/src/gleam/http/service.gleam", 52).
-spec method_override(fun((gleam@http@request:request(HDX)) -> HEF)) -> fun((gleam@http@request:request(HDX)) -> HEF).
method_override(Service) ->
    fun(Request) -> _pipe = Request,
        _pipe@1 = ensure_post(_pipe),
        _pipe@2 = gleam@result:then(_pipe@1, fun get_override_method/1),
        _pipe@3 = gleam@result:map(
            _pipe@2,
            fun(_capture) ->
                gleam@http@request:set_method(Request, _capture)
            end
        ),
        _pipe@4 = gleam@result:unwrap(_pipe@3, Request),
        Service(_pipe@4) end.
