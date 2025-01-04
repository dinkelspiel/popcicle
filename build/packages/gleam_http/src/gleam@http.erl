-module(gleam@http).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([parse_method/1, method_to_string/1, scheme_to_string/1, scheme_from_string/1, parse_content_disposition/1, parse_multipart_body/2, method_from_dynamic/1, parse_multipart_headers/2]).
-export_type([method/0, scheme/0, multipart_headers/0, multipart_body/0, content_disposition/0]).

-type method() :: get |
    post |
    head |
    put |
    delete |
    trace |
    connect |
    options |
    patch |
    {other, binary()}.

-type scheme() :: http | https.

-type multipart_headers() :: {multipart_headers,
        list({binary(), binary()}),
        bitstring()} |
    {more_required_for_headers,
        fun((bitstring()) -> {ok, multipart_headers()} | {error, nil})}.

-type multipart_body() :: {multipart_body, bitstring(), boolean(), bitstring()} |
    {more_required_for_body,
        bitstring(),
        fun((bitstring()) -> {ok, multipart_body()} | {error, nil})}.

-type content_disposition() :: {content_disposition,
        binary(),
        list({binary(), binary()})}.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 34).
-spec parse_method(binary()) -> {ok, method()} | {error, nil}.
parse_method(S) ->
    case string:lowercase(S) of
        <<"connect"/utf8>> ->
            {ok, connect};

        <<"delete"/utf8>> ->
            {ok, delete};

        <<"get"/utf8>> ->
            {ok, get};

        <<"head"/utf8>> ->
            {ok, head};

        <<"options"/utf8>> ->
            {ok, options};

        <<"patch"/utf8>> ->
            {ok, patch};

        <<"post"/utf8>> ->
            {ok, post};

        <<"put"/utf8>> ->
            {ok, put};

        <<"trace"/utf8>> ->
            {ok, trace};

        _ ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 49).
-spec method_to_string(method()) -> binary().
method_to_string(Method) ->
    case Method of
        connect ->
            <<"connect"/utf8>>;

        delete ->
            <<"delete"/utf8>>;

        get ->
            <<"get"/utf8>>;

        head ->
            <<"head"/utf8>>;

        options ->
            <<"options"/utf8>>;

        patch ->
            <<"patch"/utf8>>;

        post ->
            <<"post"/utf8>>;

        put ->
            <<"put"/utf8>>;

        trace ->
            <<"trace"/utf8>>;

        {other, S} ->
            S
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 81).
-spec scheme_to_string(scheme()) -> binary().
scheme_to_string(Scheme) ->
    case Scheme of
        http ->
            <<"http"/utf8>>;

        https ->
            <<"https"/utf8>>
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 98).
-spec scheme_from_string(binary()) -> {ok, scheme()} | {error, nil}.
scheme_from_string(Scheme) ->
    case string:lowercase(Scheme) of
        <<"http"/utf8>> ->
            {ok, http};

        <<"https"/utf8>> ->
            {ok, https};

        _ ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 337).
-spec skip_whitespace(bitstring()) -> bitstring().
skip_whitespace(Data) ->
    case Data of
        <<32, Data@1/binary>> ->
            skip_whitespace(Data@1);

        <<9, Data@1/binary>> ->
            skip_whitespace(Data@1);

        _ ->
            Data
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 427).
-spec more_please_headers(
    fun((bitstring()) -> {ok, multipart_headers()} | {error, nil}),
    bitstring()
) -> {ok, multipart_headers()} | {error, nil}.
more_please_headers(Continuation, Existing) ->
    {ok,
        {more_required_for_headers,
            fun(More) ->
                gleam@bool:guard(
                    More =:= <<>>,
                    {error, nil},
                    fun() ->
                        Continuation(<<Existing/bitstring, More/bitstring>>)
                    end
                )
            end}}.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 508).
-spec parse_rfc_2045_parameter_quoted_value(binary(), binary(), binary()) -> {ok,
        {{binary(), binary()}, binary()}} |
    {error, nil}.
parse_rfc_2045_parameter_quoted_value(Header, Name, Value) ->
    case gleam_stdlib:string_pop_grapheme(Header) of
        {error, nil} ->
            {error, nil};

        {ok, {<<"\""/utf8>>, Rest}} ->
            {ok, {{Name, Value}, Rest}};

        {ok, {<<"\\"/utf8>>, Rest@1}} ->
            gleam@result:'try'(
                gleam_stdlib:string_pop_grapheme(Rest@1),
                fun(_use0) ->
                    {Grapheme, Rest@2} = _use0,
                    parse_rfc_2045_parameter_quoted_value(
                        Rest@2,
                        Name,
                        <<Value/binary, Grapheme/binary>>
                    )
                end
            );

        {ok, {Grapheme@1, Rest@3}} ->
            parse_rfc_2045_parameter_quoted_value(
                Rest@3,
                Name,
                <<Value/binary, Grapheme@1/binary>>
            )
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 525).
-spec parse_rfc_2045_parameter_unquoted_value(binary(), binary(), binary()) -> {{binary(),
        binary()},
    binary()}.
parse_rfc_2045_parameter_unquoted_value(Header, Name, Value) ->
    case gleam_stdlib:string_pop_grapheme(Header) of
        {error, nil} ->
            {{Name, Value}, Header};

        {ok, {<<";"/utf8>>, Rest}} ->
            {{Name, Value}, Rest};

        {ok, {<<" "/utf8>>, Rest}} ->
            {{Name, Value}, Rest};

        {ok, {<<"\t"/utf8>>, Rest}} ->
            {{Name, Value}, Rest};

        {ok, {Grapheme, Rest@1}} ->
            parse_rfc_2045_parameter_unquoted_value(
                Rest@1,
                Name,
                <<Value/binary, Grapheme/binary>>
            )
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 496).
-spec parse_rfc_2045_parameter_value(binary(), binary()) -> {ok,
        {{binary(), binary()}, binary()}} |
    {error, nil}.
parse_rfc_2045_parameter_value(Header, Name) ->
    case gleam_stdlib:string_pop_grapheme(Header) of
        {error, nil} ->
            {error, nil};

        {ok, {<<"\""/utf8>>, Rest}} ->
            parse_rfc_2045_parameter_quoted_value(Rest, Name, <<""/utf8>>);

        {ok, {Grapheme, Rest@1}} ->
            {ok,
                parse_rfc_2045_parameter_unquoted_value(Rest@1, Name, Grapheme)}
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 485).
-spec parse_rfc_2045_parameter(binary(), binary()) -> {ok,
        {{binary(), binary()}, binary()}} |
    {error, nil}.
parse_rfc_2045_parameter(Header, Name) ->
    gleam@result:'try'(
        gleam_stdlib:string_pop_grapheme(Header),
        fun(_use0) ->
            {Grapheme, Rest} = _use0,
            case Grapheme of
                <<"="/utf8>> ->
                    parse_rfc_2045_parameter_value(Rest, Name);

                _ ->
                    parse_rfc_2045_parameter(
                        Rest,
                        <<Name/binary, (string:lowercase(Grapheme))/binary>>
                    )
            end
        end
    ).

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 467).
-spec parse_rfc_2045_parameters(binary(), list({binary(), binary()})) -> {ok,
        list({binary(), binary()})} |
    {error, nil}.
parse_rfc_2045_parameters(Header, Parameters) ->
    case gleam_stdlib:string_pop_grapheme(Header) of
        {error, nil} ->
            {ok, lists:reverse(Parameters)};

        {ok, {<<";"/utf8>>, Rest}} ->
            parse_rfc_2045_parameters(Rest, Parameters);

        {ok, {<<" "/utf8>>, Rest}} ->
            parse_rfc_2045_parameters(Rest, Parameters);

        {ok, {<<"\t"/utf8>>, Rest}} ->
            parse_rfc_2045_parameters(Rest, Parameters);

        {ok, {Grapheme, Rest@1}} ->
            Acc = string:lowercase(Grapheme),
            gleam@result:'try'(
                parse_rfc_2045_parameter(Rest@1, Acc),
                fun(_use0) ->
                    {Parameter, Rest@2} = _use0,
                    parse_rfc_2045_parameters(Rest@2, [Parameter | Parameters])
                end
            )
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 449).
-spec parse_content_disposition_type(binary(), binary()) -> {ok,
        content_disposition()} |
    {error, nil}.
parse_content_disposition_type(Header, Name) ->
    case gleam_stdlib:string_pop_grapheme(Header) of
        {error, nil} ->
            {ok, {content_disposition, Name, []}};

        {ok, {<<" "/utf8>>, Rest}} ->
            Result = parse_rfc_2045_parameters(Rest, []),
            gleam@result:map(
                Result,
                fun(Parameters) -> {content_disposition, Name, Parameters} end
            );

        {ok, {<<"\t"/utf8>>, Rest}} ->
            Result = parse_rfc_2045_parameters(Rest, []),
            gleam@result:map(
                Result,
                fun(Parameters) -> {content_disposition, Name, Parameters} end
            );

        {ok, {<<";"/utf8>>, Rest}} ->
            Result = parse_rfc_2045_parameters(Rest, []),
            gleam@result:map(
                Result,
                fun(Parameters) -> {content_disposition, Name, Parameters} end
            );

        {ok, {Grapheme, Rest@1}} ->
            parse_content_disposition_type(
                Rest@1,
                <<Name/binary, (string:lowercase(Grapheme))/binary>>
            )
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 443).
-spec parse_content_disposition(binary()) -> {ok, content_disposition()} |
    {error, nil}.
parse_content_disposition(Header) ->
    parse_content_disposition_type(Header, <<""/utf8>>).

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 543).
-spec more_please_body(
    fun((bitstring()) -> {ok, multipart_body()} | {error, nil}),
    bitstring(),
    bitstring()
) -> {ok, multipart_body()} | {error, nil}.
more_please_body(Continuation, Chunk, Existing) ->
    _pipe = fun(More) ->
        gleam@bool:guard(
            More =:= <<>>,
            {error, nil},
            fun() -> Continuation(<<Existing/bitstring, More/bitstring>>) end
        )
    end,
    _pipe@1 = {more_required_for_body, Chunk, _pipe},
    {ok, _pipe@1}.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 216).
-spec parse_body_loop(bitstring(), bitstring(), bitstring()) -> {ok,
        multipart_body()} |
    {error, nil}.
parse_body_loop(Data, Boundary, Body) ->
    Dsize = erlang:byte_size(Data),
    Bsize = erlang:byte_size(Boundary),
    Required = 6 + Bsize,
    case Data of
        _ when Dsize < Required ->
            more_please_body(
                fun(_capture) -> parse_body_loop(_capture, Boundary, <<>>) end,
                Body,
                Data
            );

        <<13, 10, Data@1/binary>> ->
            Desired = <<45, 45, Boundary/bitstring>>,
            Size = erlang:byte_size(Desired),
            Dsize@1 = erlang:byte_size(Data@1),
            Prefix = gleam_stdlib:bit_array_slice(Data@1, 0, Size),
            Rest = gleam_stdlib:bit_array_slice(Data@1, Size, Dsize@1 - Size),
            case {Prefix =:= {ok, Desired}, Rest} of
                {true, {ok, <<13, 10, _/binary>>}} ->
                    {ok, {multipart_body, Body, false, Data@1}};

                {true, {ok, <<45, 45, Data@2/binary>>}} ->
                    {ok, {multipart_body, Body, true, Data@2}};

                {false, _} ->
                    parse_body_loop(
                        Data@1,
                        Boundary,
                        <<Body/bitstring, 13, 10>>
                    );

                {_, _} ->
                    {error, nil}
            end;

        <<Char, Data@3/binary>> ->
            parse_body_loop(Data@3, Boundary, <<Body/bitstring, Char>>);

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"unreachable"/utf8>>,
                    module => <<"gleam/http"/utf8>>,
                    function => <<"parse_body_loop"/utf8>>,
                    line => 257})
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 204).
-spec parse_body_with_bit_array(bitstring(), bitstring()) -> {ok,
        multipart_body()} |
    {error, nil}.
parse_body_with_bit_array(Data, Boundary) ->
    Bsize = erlang:byte_size(Boundary),
    Prefix = gleam_stdlib:bit_array_slice(Data, 0, 2 + Bsize),
    case Prefix =:= {ok, <<45, 45, Boundary/bitstring>>} of
        true ->
            {ok, {multipart_body, <<>>, false, Data}};

        false ->
            parse_body_loop(Data, Boundary, <<>>)
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 195).
-spec parse_multipart_body(bitstring(), binary()) -> {ok, multipart_body()} |
    {error, nil}.
parse_multipart_body(Data, Boundary) ->
    _pipe = Boundary,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    parse_body_with_bit_array(Data, _pipe@1).

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 106).
-spec method_from_dynamic(gleam@dynamic:dynamic_()) -> {ok, method()} |
    {error, list(gleam@dynamic:decode_error())}.
method_from_dynamic(Value) ->
    case gleam_http_native:decode_method(Value) of
        {ok, Method} ->
            {ok, Method};

        {error, _} ->
            {error,
                [{decode_error,
                        <<"HTTP method"/utf8>>,
                        gleam_stdlib:classify_dynamic(Value),
                        []}]}
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 380).
-spec parse_header_value(
    bitstring(),
    list({binary(), binary()}),
    bitstring(),
    bitstring()
) -> {ok, multipart_headers()} | {error, nil}.
parse_header_value(Data, Headers, Name, Value) ->
    Size = erlang:byte_size(Data),
    case Data of
        _ when Size < 4 ->
            _pipe@2 = fun(Data@1) -> _pipe = Data@1,
                _pipe@1 = skip_whitespace(_pipe),
                parse_header_value(_pipe@1, Headers, Name, Value) end,
            more_please_headers(_pipe@2, Data);

        <<13, 10, 13, 10, Data@2/binary>> ->
            gleam@result:'try'(
                gleam@bit_array:to_string(Name),
                fun(Name@1) ->
                    gleam@result:map(
                        gleam@bit_array:to_string(Value),
                        fun(Value@1) ->
                            Headers@1 = lists:reverse(
                                [{string:lowercase(Name@1), Value@1} | Headers]
                            ),
                            {multipart_headers, Headers@1, Data@2}
                        end
                    )
                end
            );

        <<13, 10, 32, Data@3/binary>> ->
            parse_header_value(Data@3, Headers, Name, Value);

        <<13, 10, 9, Data@3/binary>> ->
            parse_header_value(Data@3, Headers, Name, Value);

        <<13, 10, Data@4/binary>> ->
            gleam@result:'try'(
                gleam@bit_array:to_string(Name),
                fun(Name@2) ->
                    gleam@result:'try'(
                        gleam@bit_array:to_string(Value),
                        fun(Value@2) ->
                            Headers@2 = [{string:lowercase(Name@2), Value@2} |
                                Headers],
                            parse_header_name(Data@4, Headers@2, <<>>)
                        end
                    )
                end
            );

        <<Char, Rest/binary>> ->
            Value@3 = <<Value/bitstring, Char>>,
            parse_header_value(Rest, Headers, Name, Value@3);

        _ ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 361).
-spec parse_header_name(bitstring(), list({binary(), binary()}), bitstring()) -> {ok,
        multipart_headers()} |
    {error, nil}.
parse_header_name(Data, Headers, Name) ->
    case skip_whitespace(Data) of
        <<58, Data@1/binary>> ->
            _pipe = Data@1,
            _pipe@1 = skip_whitespace(_pipe),
            parse_header_value(_pipe@1, Headers, Name, <<>>);

        <<Char, Data@2/binary>> ->
            parse_header_name(Data@2, Headers, <<Name/bitstring, Char>>);

        _ ->
            more_please_headers(
                fun(_capture) -> parse_header_name(_capture, Headers, Name) end,
                Data
            )
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 345).
-spec do_parse_headers(bitstring()) -> {ok, multipart_headers()} | {error, nil}.
do_parse_headers(Data) ->
    case Data of
        <<13, 10, 13, 10, Data@1/binary>> ->
            {ok, {multipart_headers, [], Data@1}};

        <<13, 10, Data@2/binary>> ->
            parse_header_name(Data@2, [], <<>>);

        <<13>> ->
            more_please_headers(fun do_parse_headers/1, Data);

        <<>> ->
            more_please_headers(fun do_parse_headers/1, Data);

        _ ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 261).
-spec parse_headers_after_prelude(bitstring(), bitstring()) -> {ok,
        multipart_headers()} |
    {error, nil}.
parse_headers_after_prelude(Data, Boundary) ->
    Dsize = erlang:byte_size(Data),
    Bsize = erlang:byte_size(Boundary),
    Required_size = Bsize + 4,
    gleam@bool:guard(
        Dsize < Required_size,
        more_please_headers(
            fun(_capture) -> parse_headers_after_prelude(_capture, Boundary) end,
            Data
        ),
        fun() ->
            gleam@result:'try'(
                gleam_stdlib:bit_array_slice(Data, 0, Required_size - 2),
                fun(Prefix) ->
                    gleam@result:'try'(
                        gleam_stdlib:bit_array_slice(Data, 2 + Bsize, 2),
                        fun(Second) ->
                            Desired = <<45, 45, Boundary/bitstring>>,
                            gleam@bool:guard(
                                Prefix /= Desired,
                                {error, nil},
                                fun() -> case Second =:= <<45, 45>> of
                                        true ->
                                            Rest_size = Dsize - Required_size,
                                            gleam@result:map(
                                                gleam_stdlib:bit_array_slice(
                                                    Data,
                                                    Required_size,
                                                    Rest_size
                                                ),
                                                fun(Data@1) ->
                                                    {multipart_headers,
                                                        [],
                                                        Data@1}
                                                end
                                            );

                                        false ->
                                            Start = Required_size - 2,
                                            Rest_size@1 = (Dsize - Required_size)
                                            + 2,
                                            gleam@result:'try'(
                                                gleam_stdlib:bit_array_slice(
                                                    Data,
                                                    Start,
                                                    Rest_size@1
                                                ),
                                                fun(Data@2) ->
                                                    do_parse_headers(Data@2)
                                                end
                                            )
                                    end end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 303).
-spec skip_preamble(bitstring(), bitstring()) -> {ok, multipart_headers()} |
    {error, nil}.
skip_preamble(Data, Boundary) ->
    Data_size = erlang:byte_size(Data),
    Boundary_size = erlang:byte_size(Boundary),
    Required = Boundary_size + 4,
    case Data of
        _ when Data_size < Required ->
            more_please_headers(
                fun(_capture) -> skip_preamble(_capture, Boundary) end,
                Data
            );

        <<13, 10, 45, 45, Data@1/binary>> ->
            case gleam_stdlib:bit_array_slice(Data@1, 0, Boundary_size) of
                {ok, Prefix} when Prefix =:= Boundary ->
                    Start = Boundary_size,
                    Length = erlang:byte_size(Data@1) - Boundary_size,
                    gleam@result:'try'(
                        gleam_stdlib:bit_array_slice(Data@1, Start, Length),
                        fun(Rest) -> do_parse_headers(Rest) end
                    );

                {ok, _} ->
                    skip_preamble(Data@1, Boundary);

                {error, _} ->
                    {error, nil}
            end;

        <<_, Data@2/binary>> ->
            skip_preamble(Data@2, Boundary);

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"unreachable"/utf8>>,
                    module => <<"gleam/http"/utf8>>,
                    function => <<"skip_preamble"/utf8>>,
                    line => 333})
    end.

-file("/Users/louis/src/gleam/http/src/gleam/http.gleam", 167).
-spec parse_multipart_headers(bitstring(), binary()) -> {ok,
        multipart_headers()} |
    {error, nil}.
parse_multipart_headers(Data, Boundary) ->
    Boundary@1 = gleam_stdlib:identity(Boundary),
    Prefix = <<45, 45, Boundary@1/bitstring>>,
    case gleam_stdlib:bit_array_slice(Data, 0, erlang:byte_size(Prefix)) =:= {ok,
        Prefix} of
        true ->
            parse_headers_after_prelude(Data, Boundary@1);

        false ->
            skip_preamble(Data, Boundary@1)
    end.
