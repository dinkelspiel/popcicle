-module(gleam@uri).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([parse/1, parse_query/1, percent_encode/1, query_to_string/1, percent_decode/1, path_segments/1, to_string/1, origin/1, merge/2]).
-export_type([uri/0]).

-type uri() :: {uri,
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(integer()),
        binary(),
        gleam@option:option(binary()),
        gleam@option:option(binary())}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 273).
-spec is_valid_host_within_brackets_char(integer()) -> boolean().
is_valid_host_within_brackets_char(Char) ->
    (((((48 >= Char) andalso (Char =< 57)) orelse ((65 >= Char) andalso (Char =< 90)))
    orelse ((97 >= Char) andalso (Char =< 122)))
    orelse (Char =:= 58))
    orelse (Char =:= 46).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 487).
-spec parse_fragment(binary(), uri()) -> {ok, uri()} | {error, nil}.
parse_fragment(Rest, Pieces) ->
    {ok,
        begin
            _record = Pieces,
            {uri,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                erlang:element(6, _record),
                erlang:element(7, _record),
                {some, Rest}}
        end}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 459).
-spec parse_query_with_question_mark_loop(binary(), binary(), uri(), integer()) -> {ok,
        uri()} |
    {error, nil}.
parse_query_with_question_mark_loop(Original, Uri_string, Pieces, Size) ->
    case Uri_string of
        <<"#"/utf8, Rest/binary>> when Size =:= 0 ->
            parse_fragment(Rest, Pieces);

        <<"#"/utf8, Rest@1/binary>> ->
            Query = binary:part(Original, 0, Size),
            Pieces@1 = begin
                _record = Pieces,
                {uri,
                    erlang:element(2, _record),
                    erlang:element(3, _record),
                    erlang:element(4, _record),
                    erlang:element(5, _record),
                    erlang:element(6, _record),
                    {some, Query},
                    erlang:element(8, _record)}
            end,
            parse_fragment(Rest@1, Pieces@1);

        <<""/utf8>> ->
            {ok,
                begin
                    _record@1 = Pieces,
                    {uri,
                        erlang:element(2, _record@1),
                        erlang:element(3, _record@1),
                        erlang:element(4, _record@1),
                        erlang:element(5, _record@1),
                        erlang:element(6, _record@1),
                        {some, Original},
                        erlang:element(8, _record@1)}
                end};

        _ ->
            {_, Rest@2} = gleam_stdlib:string_pop_codeunit(Uri_string),
            parse_query_with_question_mark_loop(
                Original,
                Rest@2,
                Pieces,
                Size + 1
            )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 452).
-spec parse_query_with_question_mark(binary(), uri()) -> {ok, uri()} |
    {error, nil}.
parse_query_with_question_mark(Uri_string, Pieces) ->
    parse_query_with_question_mark_loop(Uri_string, Uri_string, Pieces, 0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 418).
-spec parse_path_loop(binary(), binary(), uri(), integer()) -> {ok, uri()} |
    {error, nil}.
parse_path_loop(Original, Uri_string, Pieces, Size) ->
    case Uri_string of
        <<"?"/utf8, Rest/binary>> ->
            Path = binary:part(Original, 0, Size),
            Pieces@1 = begin
                _record = Pieces,
                {uri,
                    erlang:element(2, _record),
                    erlang:element(3, _record),
                    erlang:element(4, _record),
                    erlang:element(5, _record),
                    Path,
                    erlang:element(7, _record),
                    erlang:element(8, _record)}
            end,
            parse_query_with_question_mark(Rest, Pieces@1);

        <<"#"/utf8, Rest@1/binary>> ->
            Path@1 = binary:part(Original, 0, Size),
            Pieces@2 = begin
                _record@1 = Pieces,
                {uri,
                    erlang:element(2, _record@1),
                    erlang:element(3, _record@1),
                    erlang:element(4, _record@1),
                    erlang:element(5, _record@1),
                    Path@1,
                    erlang:element(7, _record@1),
                    erlang:element(8, _record@1)}
            end,
            parse_fragment(Rest@1, Pieces@2);

        <<""/utf8>> ->
            {ok,
                begin
                    _record@2 = Pieces,
                    {uri,
                        erlang:element(2, _record@2),
                        erlang:element(3, _record@2),
                        erlang:element(4, _record@2),
                        erlang:element(5, _record@2),
                        Original,
                        erlang:element(7, _record@2),
                        erlang:element(8, _record@2)}
                end};

        _ ->
            {_, Rest@2} = gleam_stdlib:string_pop_codeunit(Uri_string),
            parse_path_loop(Original, Rest@2, Pieces, Size + 1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 414).
-spec parse_path(binary(), uri()) -> {ok, uri()} | {error, nil}.
parse_path(Uri_string, Pieces) ->
    parse_path_loop(Uri_string, Uri_string, Pieces, 0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 369).
-spec parse_port_loop(binary(), uri(), integer()) -> {ok, uri()} | {error, nil}.
parse_port_loop(Uri_string, Pieces, Port) ->
    case Uri_string of
        <<"0"/utf8, Rest/binary>> ->
            parse_port_loop(Rest, Pieces, Port * 10);

        <<"1"/utf8, Rest@1/binary>> ->
            parse_port_loop(Rest@1, Pieces, (Port * 10) + 1);

        <<"2"/utf8, Rest@2/binary>> ->
            parse_port_loop(Rest@2, Pieces, (Port * 10) + 2);

        <<"3"/utf8, Rest@3/binary>> ->
            parse_port_loop(Rest@3, Pieces, (Port * 10) + 3);

        <<"4"/utf8, Rest@4/binary>> ->
            parse_port_loop(Rest@4, Pieces, (Port * 10) + 4);

        <<"5"/utf8, Rest@5/binary>> ->
            parse_port_loop(Rest@5, Pieces, (Port * 10) + 5);

        <<"6"/utf8, Rest@6/binary>> ->
            parse_port_loop(Rest@6, Pieces, (Port * 10) + 6);

        <<"7"/utf8, Rest@7/binary>> ->
            parse_port_loop(Rest@7, Pieces, (Port * 10) + 7);

        <<"8"/utf8, Rest@8/binary>> ->
            parse_port_loop(Rest@8, Pieces, (Port * 10) + 8);

        <<"9"/utf8, Rest@9/binary>> ->
            parse_port_loop(Rest@9, Pieces, (Port * 10) + 9);

        <<"?"/utf8, Rest@10/binary>> ->
            Pieces@1 = begin
                _record = Pieces,
                {uri,
                    erlang:element(2, _record),
                    erlang:element(3, _record),
                    erlang:element(4, _record),
                    {some, Port},
                    erlang:element(6, _record),
                    erlang:element(7, _record),
                    erlang:element(8, _record)}
            end,
            parse_query_with_question_mark(Rest@10, Pieces@1);

        <<"#"/utf8, Rest@11/binary>> ->
            Pieces@2 = begin
                _record@1 = Pieces,
                {uri,
                    erlang:element(2, _record@1),
                    erlang:element(3, _record@1),
                    erlang:element(4, _record@1),
                    {some, Port},
                    erlang:element(6, _record@1),
                    erlang:element(7, _record@1),
                    erlang:element(8, _record@1)}
            end,
            parse_fragment(Rest@11, Pieces@2);

        <<"/"/utf8, _/binary>> ->
            Pieces@3 = begin
                _record@2 = Pieces,
                {uri,
                    erlang:element(2, _record@2),
                    erlang:element(3, _record@2),
                    erlang:element(4, _record@2),
                    {some, Port},
                    erlang:element(6, _record@2),
                    erlang:element(7, _record@2),
                    erlang:element(8, _record@2)}
            end,
            parse_path(Uri_string, Pieces@3);

        <<""/utf8>> ->
            {ok,
                begin
                    _record@3 = Pieces,
                    {uri,
                        erlang:element(2, _record@3),
                        erlang:element(3, _record@3),
                        erlang:element(4, _record@3),
                        {some, Port},
                        erlang:element(6, _record@3),
                        erlang:element(7, _record@3),
                        erlang:element(8, _record@3)}
                end};

        _ ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 337).
-spec parse_port(binary(), uri()) -> {ok, uri()} | {error, nil}.
parse_port(Uri_string, Pieces) ->
    case Uri_string of
        <<":0"/utf8, Rest/binary>> ->
            parse_port_loop(Rest, Pieces, 0);

        <<":1"/utf8, Rest@1/binary>> ->
            parse_port_loop(Rest@1, Pieces, 1);

        <<":2"/utf8, Rest@2/binary>> ->
            parse_port_loop(Rest@2, Pieces, 2);

        <<":3"/utf8, Rest@3/binary>> ->
            parse_port_loop(Rest@3, Pieces, 3);

        <<":4"/utf8, Rest@4/binary>> ->
            parse_port_loop(Rest@4, Pieces, 4);

        <<":5"/utf8, Rest@5/binary>> ->
            parse_port_loop(Rest@5, Pieces, 5);

        <<":6"/utf8, Rest@6/binary>> ->
            parse_port_loop(Rest@6, Pieces, 6);

        <<":7"/utf8, Rest@7/binary>> ->
            parse_port_loop(Rest@7, Pieces, 7);

        <<":8"/utf8, Rest@8/binary>> ->
            parse_port_loop(Rest@8, Pieces, 8);

        <<":9"/utf8, Rest@9/binary>> ->
            parse_port_loop(Rest@9, Pieces, 9);

        <<":"/utf8, _/binary>> ->
            {error, nil};

        <<"?"/utf8, Rest@10/binary>> ->
            parse_query_with_question_mark(Rest@10, Pieces);

        <<"#"/utf8, Rest@11/binary>> ->
            parse_fragment(Rest@11, Pieces);

        <<"/"/utf8, _/binary>> ->
            parse_path(Uri_string, Pieces);

        <<""/utf8>> ->
            {ok, Pieces};

        _ ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 293).
-spec parse_host_outside_of_brackets_loop(binary(), binary(), uri(), integer()) -> {ok,
        uri()} |
    {error, nil}.
parse_host_outside_of_brackets_loop(Original, Uri_string, Pieces, Size) ->
    case Uri_string of
        <<""/utf8>> ->
            {ok,
                begin
                    _record = Pieces,
                    {uri,
                        erlang:element(2, _record),
                        erlang:element(3, _record),
                        {some, Original},
                        erlang:element(5, _record),
                        erlang:element(6, _record),
                        erlang:element(7, _record),
                        erlang:element(8, _record)}
                end};

        <<":"/utf8, _/binary>> ->
            Host = binary:part(Original, 0, Size),
            Pieces@1 = begin
                _record@1 = Pieces,
                {uri,
                    erlang:element(2, _record@1),
                    erlang:element(3, _record@1),
                    {some, Host},
                    erlang:element(5, _record@1),
                    erlang:element(6, _record@1),
                    erlang:element(7, _record@1),
                    erlang:element(8, _record@1)}
            end,
            parse_port(Uri_string, Pieces@1);

        <<"/"/utf8, _/binary>> ->
            Host@1 = binary:part(Original, 0, Size),
            Pieces@2 = begin
                _record@2 = Pieces,
                {uri,
                    erlang:element(2, _record@2),
                    erlang:element(3, _record@2),
                    {some, Host@1},
                    erlang:element(5, _record@2),
                    erlang:element(6, _record@2),
                    erlang:element(7, _record@2),
                    erlang:element(8, _record@2)}
            end,
            parse_path(Uri_string, Pieces@2);

        <<"?"/utf8, Rest/binary>> ->
            Host@2 = binary:part(Original, 0, Size),
            Pieces@3 = begin
                _record@3 = Pieces,
                {uri,
                    erlang:element(2, _record@3),
                    erlang:element(3, _record@3),
                    {some, Host@2},
                    erlang:element(5, _record@3),
                    erlang:element(6, _record@3),
                    erlang:element(7, _record@3),
                    erlang:element(8, _record@3)}
            end,
            parse_query_with_question_mark(Rest, Pieces@3);

        <<"#"/utf8, Rest@1/binary>> ->
            Host@3 = binary:part(Original, 0, Size),
            Pieces@4 = begin
                _record@4 = Pieces,
                {uri,
                    erlang:element(2, _record@4),
                    erlang:element(3, _record@4),
                    {some, Host@3},
                    erlang:element(5, _record@4),
                    erlang:element(6, _record@4),
                    erlang:element(7, _record@4),
                    erlang:element(8, _record@4)}
            end,
            parse_fragment(Rest@1, Pieces@4);

        _ ->
            {_, Rest@2} = gleam_stdlib:string_pop_codeunit(Uri_string),
            parse_host_outside_of_brackets_loop(
                Original,
                Rest@2,
                Pieces,
                Size + 1
            )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 213).
-spec parse_host_within_brackets_loop(binary(), binary(), uri(), integer()) -> {ok,
        uri()} |
    {error, nil}.
parse_host_within_brackets_loop(Original, Uri_string, Pieces, Size) ->
    case Uri_string of
        <<""/utf8>> ->
            {ok,
                begin
                    _record = Pieces,
                    {uri,
                        erlang:element(2, _record),
                        erlang:element(3, _record),
                        {some, Uri_string},
                        erlang:element(5, _record),
                        erlang:element(6, _record),
                        erlang:element(7, _record),
                        erlang:element(8, _record)}
                end};

        <<"]"/utf8, Rest/binary>> when Size =:= 0 ->
            parse_port(Rest, Pieces);

        <<"]"/utf8, Rest@1/binary>> ->
            Host = binary:part(Original, 0, Size + 1),
            Pieces@1 = begin
                _record@1 = Pieces,
                {uri,
                    erlang:element(2, _record@1),
                    erlang:element(3, _record@1),
                    {some, Host},
                    erlang:element(5, _record@1),
                    erlang:element(6, _record@1),
                    erlang:element(7, _record@1),
                    erlang:element(8, _record@1)}
            end,
            parse_port(Rest@1, Pieces@1);

        <<"/"/utf8, _/binary>> when Size =:= 0 ->
            parse_path(Uri_string, Pieces);

        <<"/"/utf8, _/binary>> ->
            Host@1 = binary:part(Original, 0, Size),
            Pieces@2 = begin
                _record@2 = Pieces,
                {uri,
                    erlang:element(2, _record@2),
                    erlang:element(3, _record@2),
                    {some, Host@1},
                    erlang:element(5, _record@2),
                    erlang:element(6, _record@2),
                    erlang:element(7, _record@2),
                    erlang:element(8, _record@2)}
            end,
            parse_path(Uri_string, Pieces@2);

        <<"?"/utf8, Rest@2/binary>> when Size =:= 0 ->
            parse_query_with_question_mark(Rest@2, Pieces);

        <<"?"/utf8, Rest@3/binary>> ->
            Host@2 = binary:part(Original, 0, Size),
            Pieces@3 = begin
                _record@3 = Pieces,
                {uri,
                    erlang:element(2, _record@3),
                    erlang:element(3, _record@3),
                    {some, Host@2},
                    erlang:element(5, _record@3),
                    erlang:element(6, _record@3),
                    erlang:element(7, _record@3),
                    erlang:element(8, _record@3)}
            end,
            parse_query_with_question_mark(Rest@3, Pieces@3);

        <<"#"/utf8, Rest@4/binary>> when Size =:= 0 ->
            parse_fragment(Rest@4, Pieces);

        <<"#"/utf8, Rest@5/binary>> ->
            Host@3 = binary:part(Original, 0, Size),
            Pieces@4 = begin
                _record@4 = Pieces,
                {uri,
                    erlang:element(2, _record@4),
                    erlang:element(3, _record@4),
                    {some, Host@3},
                    erlang:element(5, _record@4),
                    erlang:element(6, _record@4),
                    erlang:element(7, _record@4),
                    erlang:element(8, _record@4)}
            end,
            parse_fragment(Rest@5, Pieces@4);

        _ ->
            {Char, Rest@6} = gleam_stdlib:string_pop_codeunit(Uri_string),
            case is_valid_host_within_brackets_char(Char) of
                true ->
                    parse_host_within_brackets_loop(
                        Original,
                        Rest@6,
                        Pieces,
                        Size + 1
                    );

                false ->
                    parse_host_outside_of_brackets_loop(
                        Original,
                        Original,
                        Pieces,
                        0
                    )
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 206).
-spec parse_host_within_brackets(binary(), uri()) -> {ok, uri()} | {error, nil}.
parse_host_within_brackets(Uri_string, Pieces) ->
    parse_host_within_brackets_loop(Uri_string, Uri_string, Pieces, 0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 286).
-spec parse_host_outside_of_brackets(binary(), uri()) -> {ok, uri()} |
    {error, nil}.
parse_host_outside_of_brackets(Uri_string, Pieces) ->
    parse_host_outside_of_brackets_loop(Uri_string, Uri_string, Pieces, 0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 183).
-spec parse_host(binary(), uri()) -> {ok, uri()} | {error, nil}.
parse_host(Uri_string, Pieces) ->
    case Uri_string of
        <<"["/utf8, _/binary>> ->
            parse_host_within_brackets(Uri_string, Pieces);

        <<":"/utf8, _/binary>> ->
            Pieces@1 = begin
                _record = Pieces,
                {uri,
                    erlang:element(2, _record),
                    erlang:element(3, _record),
                    {some, <<""/utf8>>},
                    erlang:element(5, _record),
                    erlang:element(6, _record),
                    erlang:element(7, _record),
                    erlang:element(8, _record)}
            end,
            parse_port(Uri_string, Pieces@1);

        <<""/utf8>> ->
            {ok,
                begin
                    _record@1 = Pieces,
                    {uri,
                        erlang:element(2, _record@1),
                        erlang:element(3, _record@1),
                        {some, <<""/utf8>>},
                        erlang:element(5, _record@1),
                        erlang:element(6, _record@1),
                        erlang:element(7, _record@1),
                        erlang:element(8, _record@1)}
                end};

        _ ->
            parse_host_outside_of_brackets(Uri_string, Pieces)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 151).
-spec parse_userinfo_loop(binary(), binary(), uri(), integer()) -> {ok, uri()} |
    {error, nil}.
parse_userinfo_loop(Original, Uri_string, Pieces, Size) ->
    case Uri_string of
        <<"@"/utf8, Rest/binary>> when Size =:= 0 ->
            parse_host(Rest, Pieces);

        <<"@"/utf8, Rest@1/binary>> ->
            Userinfo = binary:part(Original, 0, Size),
            Pieces@1 = begin
                _record = Pieces,
                {uri,
                    erlang:element(2, _record),
                    {some, Userinfo},
                    erlang:element(4, _record),
                    erlang:element(5, _record),
                    erlang:element(6, _record),
                    erlang:element(7, _record),
                    erlang:element(8, _record)}
            end,
            parse_host(Rest@1, Pieces@1);

        <<""/utf8>> ->
            parse_host(Original, Pieces);

        <<"/"/utf8, _/binary>> ->
            parse_host(Original, Pieces);

        <<"?"/utf8, _/binary>> ->
            parse_host(Original, Pieces);

        <<"#"/utf8, _/binary>> ->
            parse_host(Original, Pieces);

        _ ->
            {_, Rest@2} = gleam_stdlib:string_pop_codeunit(Uri_string),
            parse_userinfo_loop(Original, Rest@2, Pieces, Size + 1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 147).
-spec parse_authority_pieces(binary(), uri()) -> {ok, uri()} | {error, nil}.
parse_authority_pieces(String, Pieces) ->
    parse_userinfo_loop(String, String, Pieces, 0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 134).
-spec parse_authority_with_slashes(binary(), uri()) -> {ok, uri()} |
    {error, nil}.
parse_authority_with_slashes(Uri_string, Pieces) ->
    case Uri_string of
        <<"//"/utf8>> ->
            {ok,
                begin
                    _record = Pieces,
                    {uri,
                        erlang:element(2, _record),
                        erlang:element(3, _record),
                        {some, <<""/utf8>>},
                        erlang:element(5, _record),
                        erlang:element(6, _record),
                        erlang:element(7, _record),
                        erlang:element(8, _record)}
                end};

        <<"//"/utf8, Rest/binary>> ->
            parse_authority_pieces(Rest, Pieces);

        _ ->
            parse_path(Uri_string, Pieces)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 75).
-spec parse_scheme_loop(binary(), binary(), uri(), integer()) -> {ok, uri()} |
    {error, nil}.
parse_scheme_loop(Original, Uri_string, Pieces, Size) ->
    case Uri_string of
        <<"/"/utf8, _/binary>> when Size =:= 0 ->
            parse_authority_with_slashes(Uri_string, Pieces);

        <<"/"/utf8, _/binary>> ->
            Scheme = binary:part(Original, 0, Size),
            Pieces@1 = begin
                _record = Pieces,
                {uri,
                    {some, string:lowercase(Scheme)},
                    erlang:element(3, _record),
                    erlang:element(4, _record),
                    erlang:element(5, _record),
                    erlang:element(6, _record),
                    erlang:element(7, _record),
                    erlang:element(8, _record)}
            end,
            parse_authority_with_slashes(Uri_string, Pieces@1);

        <<"?"/utf8, Rest/binary>> when Size =:= 0 ->
            parse_query_with_question_mark(Rest, Pieces);

        <<"?"/utf8, Rest@1/binary>> ->
            Scheme@1 = binary:part(Original, 0, Size),
            Pieces@2 = begin
                _record@1 = Pieces,
                {uri,
                    {some, string:lowercase(Scheme@1)},
                    erlang:element(3, _record@1),
                    erlang:element(4, _record@1),
                    erlang:element(5, _record@1),
                    erlang:element(6, _record@1),
                    erlang:element(7, _record@1),
                    erlang:element(8, _record@1)}
            end,
            parse_query_with_question_mark(Rest@1, Pieces@2);

        <<"#"/utf8, Rest@2/binary>> when Size =:= 0 ->
            parse_fragment(Rest@2, Pieces);

        <<"#"/utf8, Rest@3/binary>> ->
            Scheme@2 = binary:part(Original, 0, Size),
            Pieces@3 = begin
                _record@2 = Pieces,
                {uri,
                    {some, string:lowercase(Scheme@2)},
                    erlang:element(3, _record@2),
                    erlang:element(4, _record@2),
                    erlang:element(5, _record@2),
                    erlang:element(6, _record@2),
                    erlang:element(7, _record@2),
                    erlang:element(8, _record@2)}
            end,
            parse_fragment(Rest@3, Pieces@3);

        <<":"/utf8, _/binary>> when Size =:= 0 ->
            {error, nil};

        <<":"/utf8, Rest@4/binary>> ->
            Scheme@3 = binary:part(Original, 0, Size),
            Pieces@4 = begin
                _record@3 = Pieces,
                {uri,
                    {some, string:lowercase(Scheme@3)},
                    erlang:element(3, _record@3),
                    erlang:element(4, _record@3),
                    erlang:element(5, _record@3),
                    erlang:element(6, _record@3),
                    erlang:element(7, _record@3),
                    erlang:element(8, _record@3)}
            end,
            parse_authority_with_slashes(Rest@4, Pieces@4);

        <<""/utf8>> ->
            {ok,
                begin
                    _record@4 = Pieces,
                    {uri,
                        erlang:element(2, _record@4),
                        erlang:element(3, _record@4),
                        erlang:element(4, _record@4),
                        erlang:element(5, _record@4),
                        Original,
                        erlang:element(7, _record@4),
                        erlang:element(8, _record@4)}
                end};

        _ ->
            {_, Rest@5} = gleam_stdlib:string_pop_codeunit(Uri_string),
            parse_scheme_loop(Original, Rest@5, Pieces, Size + 1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 54).
-spec parse(binary()) -> {ok, uri()} | {error, nil}.
parse(Uri_string) ->
    gleam_stdlib:uri_parse(Uri_string).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 504).
-spec extra_required(list(any()), integer()) -> integer().
extra_required(List, Remaining) ->
    case List of
        _ when Remaining =:= 0 ->
            0;

        [] ->
            Remaining;

        [_ | Rest] ->
            extra_required(Rest, Remaining - 1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 526).
-spec parse_query(binary()) -> {ok, list({binary(), binary()})} | {error, nil}.
parse_query(Query) ->
    gleam_stdlib:parse_query(Query).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 562).
-spec percent_encode(binary()) -> binary().
percent_encode(Value) ->
    gleam_stdlib:percent_encode(Value).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 547).
-spec query_pair({binary(), binary()}) -> gleam@string_tree:string_tree().
query_pair(Pair) ->
    gleam_stdlib:identity(
        [gleam_stdlib:percent_encode(erlang:element(1, Pair)),
            <<"="/utf8>>,
            gleam_stdlib:percent_encode(erlang:element(2, Pair))]
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 539).
-spec query_to_string(list({binary(), binary()})) -> binary().
query_to_string(Query) ->
    _pipe = Query,
    _pipe@1 = gleam@list:map(_pipe, fun query_pair/1),
    _pipe@2 = gleam@list:intersperse(
        _pipe@1,
        gleam_stdlib:identity(<<"&"/utf8>>)
    ),
    _pipe@3 = gleam_stdlib:identity(_pipe@2),
    unicode:characters_to_binary(_pipe@3).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 575).
-spec percent_decode(binary()) -> {ok, binary()} | {error, nil}.
percent_decode(Value) ->
    gleam_stdlib:percent_decode(Value).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 597).
-spec remove_dot_segments_loop(list(binary()), list(binary())) -> list(binary()).
remove_dot_segments_loop(Input, Accumulator) ->
    case Input of
        [] ->
            lists:reverse(Accumulator);

        [Segment | Rest] ->
            Accumulator@5 = case {Segment, Accumulator} of
                {<<""/utf8>>, Accumulator@1} ->
                    Accumulator@1;

                {<<"."/utf8>>, Accumulator@2} ->
                    Accumulator@2;

                {<<".."/utf8>>, []} ->
                    [];

                {<<".."/utf8>>, [_ | Accumulator@3]} ->
                    Accumulator@3;

                {Segment@1, Accumulator@4} ->
                    [Segment@1 | Accumulator@4]
            end,
            remove_dot_segments_loop(Rest, Accumulator@5)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 593).
-spec remove_dot_segments(list(binary())) -> list(binary()).
remove_dot_segments(Input) ->
    remove_dot_segments_loop(Input, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 589).
-spec path_segments(binary()) -> list(binary()).
path_segments(Path) ->
    remove_dot_segments(gleam@string:split(Path, <<"/"/utf8>>)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 628).
-spec to_string(uri()) -> binary().
to_string(Uri) ->
    Parts = case erlang:element(8, Uri) of
        {some, Fragment} ->
            [<<"#"/utf8>>, Fragment];

        _ ->
            []
    end,
    Parts@1 = case erlang:element(7, Uri) of
        {some, Query} ->
            [<<"?"/utf8>>, Query | Parts];

        _ ->
            Parts
    end,
    Parts@2 = [erlang:element(6, Uri) | Parts@1],
    Parts@3 = case {erlang:element(4, Uri),
        gleam_stdlib:string_starts_with(erlang:element(6, Uri), <<"/"/utf8>>)} of
        {{some, Host}, false} when Host =/= <<""/utf8>> ->
            [<<"/"/utf8>> | Parts@2];

        {_, _} ->
            Parts@2
    end,
    Parts@4 = case {erlang:element(4, Uri), erlang:element(5, Uri)} of
        {{some, _}, {some, Port}} ->
            [<<":"/utf8>>, erlang:integer_to_binary(Port) | Parts@3];

        {_, _} ->
            Parts@3
    end,
    Parts@5 = case {erlang:element(2, Uri),
        erlang:element(3, Uri),
        erlang:element(4, Uri)} of
        {{some, S}, {some, U}, {some, H}} ->
            [S, <<"://"/utf8>>, U, <<"@"/utf8>>, H | Parts@4];

        {{some, S@1}, none, {some, H@1}} ->
            [S@1, <<"://"/utf8>>, H@1 | Parts@4];

        {{some, S@2}, {some, _}, none} ->
            [S@2, <<":"/utf8>> | Parts@4];

        {{some, S@2}, none, none} ->
            [S@2, <<":"/utf8>> | Parts@4];

        {none, none, {some, H@2}} ->
            [<<"//"/utf8>>, H@2 | Parts@4];

        {_, _, _} ->
            Parts@4
    end,
    gleam@string:concat(Parts@5).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 672).
-spec origin(uri()) -> {ok, binary()} | {error, nil}.
origin(Uri) ->
    {uri, Scheme, _, Host, Port, _, _, _} = Uri,
    case {Host, Scheme} of
        {{some, H}, {some, <<"https"/utf8>>}} when Port =:= {some, 443} ->
            {ok, gleam@string:concat([<<"https://"/utf8>>, H])};

        {{some, H@1}, {some, <<"http"/utf8>>}} when Port =:= {some, 80} ->
            {ok, gleam@string:concat([<<"http://"/utf8>>, H@1])};

        {{some, H@2}, {some, S}} when (S =:= <<"http"/utf8>>) orelse (S =:= <<"https"/utf8>>) ->
            case Port of
                {some, P} ->
                    {ok,
                        gleam@string:concat(
                            [S,
                                <<"://"/utf8>>,
                                H@2,
                                <<":"/utf8>>,
                                erlang:integer_to_binary(P)]
                        )};

                none ->
                    {ok, gleam@string:concat([S, <<"://"/utf8>>, H@2])}
            end;

        {_, _} ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 751).
-spec drop_last(list(ERW)) -> list(ERW).
drop_last(Elements) ->
    gleam@list:take(Elements, erlang:length(Elements) - 1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 755).
-spec join_segments(list(binary())) -> binary().
join_segments(Segments) ->
    gleam@string:join([<<""/utf8>> | Segments], <<"/"/utf8>>).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 695).
-spec merge(uri(), uri()) -> {ok, uri()} | {error, nil}.
merge(Base, Relative) ->
    case Base of
        {uri, {some, _}, _, {some, _}, _, _, _, _} ->
            case Relative of
                {uri, _, _, {some, _}, _, _, _, _} ->
                    Path = begin
                        _pipe = gleam@string:split(
                            erlang:element(6, Relative),
                            <<"/"/utf8>>
                        ),
                        _pipe@1 = remove_dot_segments(_pipe),
                        join_segments(_pipe@1)
                    end,
                    Resolved = {uri,
                        gleam@option:'or'(
                            erlang:element(2, Relative),
                            erlang:element(2, Base)
                        ),
                        none,
                        erlang:element(4, Relative),
                        gleam@option:'or'(
                            erlang:element(5, Relative),
                            erlang:element(5, Base)
                        ),
                        Path,
                        erlang:element(7, Relative),
                        erlang:element(8, Relative)},
                    {ok, Resolved};

                _ ->
                    {New_path, New_query} = case erlang:element(6, Relative) of
                        <<""/utf8>> ->
                            {erlang:element(6, Base),
                                gleam@option:'or'(
                                    erlang:element(7, Relative),
                                    erlang:element(7, Base)
                                )};

                        _ ->
                            Path_segments = case gleam_stdlib:string_starts_with(
                                erlang:element(6, Relative),
                                <<"/"/utf8>>
                            ) of
                                true ->
                                    gleam@string:split(
                                        erlang:element(6, Relative),
                                        <<"/"/utf8>>
                                    );

                                false ->
                                    _pipe@2 = gleam@string:split(
                                        erlang:element(6, Base),
                                        <<"/"/utf8>>
                                    ),
                                    _pipe@3 = drop_last(_pipe@2),
                                    lists:append(
                                        _pipe@3,
                                        gleam@string:split(
                                            erlang:element(6, Relative),
                                            <<"/"/utf8>>
                                        )
                                    )
                            end,
                            Path@1 = begin
                                _pipe@4 = Path_segments,
                                _pipe@5 = remove_dot_segments(_pipe@4),
                                join_segments(_pipe@5)
                            end,
                            {Path@1, erlang:element(7, Relative)}
                    end,
                    Resolved@1 = {uri,
                        erlang:element(2, Base),
                        none,
                        erlang:element(4, Base),
                        erlang:element(5, Base),
                        New_path,
                        New_query,
                        erlang:element(8, Relative)},
                    {ok, Resolved@1}
            end;

        _ ->
            {error, nil}
    end.
