-module(lustre@internals@escape).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([escape/1]).

-file("/home/runner/work/lustre/lustre/src/lustre/internals/escape.gleam", 83).
-spec do_escape_normal(
    bitstring(),
    integer(),
    bitstring(),
    list(bitstring()),
    integer()
) -> list(bitstring()).
do_escape_normal(Bin, Skip, Original, Acc, Len) ->
    case Bin of
        <<"<"/utf8, Rest/bitstring>> ->
            _assert_subject = gleam_stdlib:bit_array_slice(Original, Skip, Len),
            {ok, Slice} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"lustre/internals/escape"/utf8>>,
                                function => <<"do_escape_normal"/utf8>>,
                                line => 114})
            end,
            Acc@1 = [<<"&lt;"/utf8>>, Slice | Acc],
            do_escape(Rest, (Skip + Len) + 1, Original, Acc@1);

        <<">"/utf8, Rest@1/bitstring>> ->
            _assert_subject@1 = gleam_stdlib:bit_array_slice(
                Original,
                Skip,
                Len
            ),
            {ok, Slice@1} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"lustre/internals/escape"/utf8>>,
                                function => <<"do_escape_normal"/utf8>>,
                                line => 120})
            end,
            Acc@2 = [<<"&gt;"/utf8>>, Slice@1 | Acc],
            do_escape(Rest@1, (Skip + Len) + 1, Original, Acc@2);

        <<"&"/utf8, Rest@2/bitstring>> ->
            _assert_subject@2 = gleam_stdlib:bit_array_slice(
                Original,
                Skip,
                Len
            ),
            {ok, Slice@2} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@2,
                                module => <<"lustre/internals/escape"/utf8>>,
                                function => <<"do_escape_normal"/utf8>>,
                                line => 126})
            end,
            Acc@3 = [<<"&amp;"/utf8>>, Slice@2 | Acc],
            do_escape(Rest@2, (Skip + Len) + 1, Original, Acc@3);

        <<"\""/utf8, Rest@3/bitstring>> ->
            _assert_subject@3 = gleam_stdlib:bit_array_slice(
                Original,
                Skip,
                Len
            ),
            {ok, Slice@3} = case _assert_subject@3 of
                {ok, _} -> _assert_subject@3;
                _assert_fail@3 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@3,
                                module => <<"lustre/internals/escape"/utf8>>,
                                function => <<"do_escape_normal"/utf8>>,
                                line => 132})
            end,
            Acc@4 = [<<"&quot;"/utf8>>, Slice@3 | Acc],
            do_escape(Rest@3, (Skip + Len) + 1, Original, Acc@4);

        <<"'"/utf8, Rest@4/bitstring>> ->
            _assert_subject@4 = gleam_stdlib:bit_array_slice(
                Original,
                Skip,
                Len
            ),
            {ok, Slice@4} = case _assert_subject@4 of
                {ok, _} -> _assert_subject@4;
                _assert_fail@4 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@4,
                                module => <<"lustre/internals/escape"/utf8>>,
                                function => <<"do_escape_normal"/utf8>>,
                                line => 138})
            end,
            Acc@5 = [<<"&#39;"/utf8>>, Slice@4 | Acc],
            do_escape(Rest@4, (Skip + Len) + 1, Original, Acc@5);

        <<_, Rest@5/bitstring>> ->
            do_escape_normal(Rest@5, Skip, Original, Acc, Len + 1);

        <<>> ->
            case Skip of
                0 ->
                    [Original];

                _ ->
                    _assert_subject@5 = gleam_stdlib:bit_array_slice(
                        Original,
                        Skip,
                        Len
                    ),
                    {ok, Slice@5} = case _assert_subject@5 of
                        {ok, _} -> _assert_subject@5;
                        _assert_fail@5 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        value => _assert_fail@5,
                                        module => <<"lustre/internals/escape"/utf8>>,
                                        function => <<"do_escape_normal"/utf8>>,
                                        line => 151})
                    end,
                    [Slice@5 | Acc]
            end;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"non byte aligned string, all strings should be byte aligned"/utf8>>,
                    module => <<"lustre/internals/escape"/utf8>>,
                    function => <<"do_escape_normal"/utf8>>,
                    line => 156})
    end.

-file("/home/runner/work/lustre/lustre/src/lustre/internals/escape.gleam", 37).
-spec do_escape(bitstring(), integer(), bitstring(), list(bitstring())) -> list(bitstring()).
do_escape(Bin, Skip, Original, Acc) ->
    case Bin of
        <<"<"/utf8, Rest/bitstring>> ->
            Acc@1 = [<<"&lt;"/utf8>> | Acc],
            do_escape(Rest, Skip + 1, Original, Acc@1);

        <<">"/utf8, Rest@1/bitstring>> ->
            Acc@2 = [<<"&gt;"/utf8>> | Acc],
            do_escape(Rest@1, Skip + 1, Original, Acc@2);

        <<"&"/utf8, Rest@2/bitstring>> ->
            Acc@3 = [<<"&amp;"/utf8>> | Acc],
            do_escape(Rest@2, Skip + 1, Original, Acc@3);

        <<"\""/utf8, Rest@3/bitstring>> ->
            Acc@4 = [<<"&quot;"/utf8>> | Acc],
            do_escape(Rest@3, Skip + 1, Original, Acc@4);

        <<"'"/utf8, Rest@4/bitstring>> ->
            Acc@5 = [<<"&#39;"/utf8>> | Acc],
            do_escape(Rest@4, Skip + 1, Original, Acc@5);

        <<_, Rest@5/bitstring>> ->
            do_escape_normal(Rest@5, Skip, Original, Acc, 1);

        <<>> ->
            Acc;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"non byte aligned string, all strings should be byte aligned"/utf8>>,
                    module => <<"lustre/internals/escape"/utf8>>,
                    function => <<"do_escape"/utf8>>,
                    line => 78})
    end.

-file("/home/runner/work/lustre/lustre/src/lustre/internals/escape.gleam", 9).
-spec escape(binary()) -> binary().
escape(Text) ->
    Bits = <<Text/binary>>,
    Acc = do_escape(Bits, 0, Bits, []),
    _pipe = lists:reverse(Acc),
    _pipe@1 = gleam_stdlib:bit_array_concat(_pipe),
    lustre_escape_ffi:coerce(_pipe@1).
