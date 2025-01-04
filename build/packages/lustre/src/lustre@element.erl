-module(lustre@element).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([element/3, keyed/2, namespaced/4, advanced/6, text/1, none/0, fragment/1, map/2, get_root/1, to_string/1, to_document_string/1, to_string_builder/1, to_document_string_builder/1, to_readable_string/1]).

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 90).
-spec element(
    binary(),
    list(lustre@internals@vdom:attribute(PHA)),
    list(lustre@internals@vdom:element(PHA))
) -> lustre@internals@vdom:element(PHA).
element(Tag, Attrs, Children) ->
    case Tag of
        <<"area"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"base"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"br"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"col"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"embed"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"hr"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"img"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"input"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"link"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"meta"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"param"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"source"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"track"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"wbr"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        _ ->
            {element,
                <<""/utf8>>,
                <<""/utf8>>,
                Tag,
                Attrs,
                Children,
                false,
                false}
    end.

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 174).
-spec do_keyed(lustre@internals@vdom:element(PHN), binary()) -> lustre@internals@vdom:element(PHN).
do_keyed(El, Key) ->
    case El of
        {element, _, Namespace, Tag, Attrs, Children, Self_closing, Void} ->
            {element, Key, Namespace, Tag, Attrs, Children, Self_closing, Void};

        {map, Subtree} ->
            {map, fun() -> do_keyed(Subtree(), Key) end};

        _ ->
            El
    end.

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 164).
-spec keyed(
    fun((list(lustre@internals@vdom:element(PHG))) -> lustre@internals@vdom:element(PHG)),
    list({binary(), lustre@internals@vdom:element(PHG)})
) -> lustre@internals@vdom:element(PHG).
keyed(El, Children) ->
    El(
        (gleam@list:map(
            Children,
            fun(_use0) ->
                {Key, Child} = _use0,
                do_keyed(Child, Key)
            end
        ))
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 195).
-spec namespaced(
    binary(),
    binary(),
    list(lustre@internals@vdom:attribute(PHQ)),
    list(lustre@internals@vdom:element(PHQ))
) -> lustre@internals@vdom:element(PHQ).
namespaced(Namespace, Tag, Attrs, Children) ->
    {element, <<""/utf8>>, Namespace, Tag, Attrs, Children, false, false}.

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 217).
-spec advanced(
    binary(),
    binary(),
    list(lustre@internals@vdom:attribute(PHW)),
    list(lustre@internals@vdom:element(PHW)),
    boolean(),
    boolean()
) -> lustre@internals@vdom:element(PHW).
advanced(Namespace, Tag, Attrs, Children, Self_closing, Void) ->
    {element, <<""/utf8>>, Namespace, Tag, Attrs, Children, Self_closing, Void}.

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 241).
-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    {text, Content}.

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 249).
-spec none() -> lustre@internals@vdom:element(any()).
none() ->
    {text, <<""/utf8>>}.

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 258).
-spec fragment(list(lustre@internals@vdom:element(PIG))) -> lustre@internals@vdom:element(PIG).
fragment(Elements) ->
    element(
        <<"lustre-fragment"/utf8>>,
        [lustre@attribute:style([{<<"display"/utf8>>, <<"contents"/utf8>>}])],
        Elements
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 275).
-spec map(lustre@internals@vdom:element(PIK), fun((PIK) -> PIM)) -> lustre@internals@vdom:element(PIM).
map(Element, F) ->
    case Element of
        {text, Content} ->
            {text, Content};

        {map, Subtree} ->
            {map, fun() -> map(Subtree(), F) end};

        {element, Key, Namespace, Tag, Attrs, Children, Self_closing, Void} ->
            {map,
                fun() ->
                    {element,
                        Key,
                        Namespace,
                        Tag,
                        gleam@list:map(
                            Attrs,
                            fun(_capture) ->
                                lustre@attribute:map(_capture, F)
                            end
                        ),
                        gleam@list:map(
                            Children,
                            fun(_capture@1) -> map(_capture@1, F) end
                        ),
                        Self_closing,
                        Void}
                end}
    end.

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 297).
-spec get_root(fun((fun((PIO) -> nil), gleam@dynamic:dynamic_()) -> nil)) -> lustre@effect:effect(PIO).
get_root(Effect) ->
    lustre@effect:custom(
        fun(Dispatch, _, _, Root) -> Effect(Dispatch, Root) end
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 311).
-spec to_string(lustre@internals@vdom:element(any())) -> binary().
to_string(Element) ->
    lustre@internals@vdom:element_to_string(Element).

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 322).
-spec to_document_string(lustre@internals@vdom:element(any())) -> binary().
to_document_string(El) ->
    _pipe = lustre@internals@vdom:element_to_string(case El of
            {element, _, _, <<"html"/utf8>>, _, _, _, _} ->
                El;

            {element, _, _, <<"head"/utf8>>, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            {element, _, _, <<"body"/utf8>>, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            {map, Subtree} ->
                Subtree();

            _ ->
                element(
                    <<"html"/utf8>>,
                    [],
                    [element(<<"body"/utf8>>, [], [El])]
                )
        end),
    gleam@string:append(<<"<!doctype html>\n"/utf8>>, _pipe).

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 340).
-spec to_string_builder(lustre@internals@vdom:element(any())) -> gleam@string_tree:string_tree().
to_string_builder(Element) ->
    lustre@internals@vdom:element_to_string_builder(Element).

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 351).
-spec to_document_string_builder(lustre@internals@vdom:element(any())) -> gleam@string_tree:string_tree().
to_document_string_builder(El) ->
    _pipe = lustre@internals@vdom:element_to_string_builder(case El of
            {element, _, _, <<"html"/utf8>>, _, _, _, _} ->
                El;

            {element, _, _, <<"head"/utf8>>, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            {element, _, _, <<"body"/utf8>>, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            {map, Subtree} ->
                Subtree();

            _ ->
                element(
                    <<"html"/utf8>>,
                    [],
                    [element(<<"body"/utf8>>, [], [El])]
                )
        end),
    gleam@string_tree:prepend(_pipe, <<"<!doctype html>\n"/utf8>>).

-file("/home/runner/work/lustre/lustre/src/lustre/element.gleam", 387).
-spec to_readable_string(lustre@internals@vdom:element(any())) -> binary().
to_readable_string(El) ->
    lustre@internals@vdom:element_to_snapshot(El).
