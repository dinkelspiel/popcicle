-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([html/2, text/1, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/2, details/2, dialog/2, summary/2, slot/1, template/2]).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 9).
-spec html(
    list(lustre@internals@vdom:attribute(RET)),
    list(lustre@internals@vdom:element(RET))
) -> lustre@internals@vdom:element(RET).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 16).
-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    lustre@element:text(Content).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 23).
-spec base(list(lustre@internals@vdom:attribute(RFB))) -> lustre@internals@vdom:element(RFB).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 28).
-spec head(
    list(lustre@internals@vdom:attribute(RFF)),
    list(lustre@internals@vdom:element(RFF))
) -> lustre@internals@vdom:element(RFF).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 36).
-spec link(list(lustre@internals@vdom:attribute(RFL))) -> lustre@internals@vdom:element(RFL).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 41).
-spec meta(list(lustre@internals@vdom:attribute(RFP))) -> lustre@internals@vdom:element(RFP).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 46).
-spec style(list(lustre@internals@vdom:attribute(RFT)), binary()) -> lustre@internals@vdom:element(RFT).
style(Attrs, Css) ->
    lustre@element:element(<<"style"/utf8>>, Attrs, [text(Css)]).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 51).
-spec title(list(lustre@internals@vdom:attribute(RFX)), binary()) -> lustre@internals@vdom:element(RFX).
title(Attrs, Content) ->
    lustre@element:element(<<"title"/utf8>>, Attrs, [text(Content)]).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 58).
-spec body(
    list(lustre@internals@vdom:attribute(RGB)),
    list(lustre@internals@vdom:element(RGB))
) -> lustre@internals@vdom:element(RGB).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 68).
-spec address(
    list(lustre@internals@vdom:attribute(RGH)),
    list(lustre@internals@vdom:element(RGH))
) -> lustre@internals@vdom:element(RGH).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 76).
-spec article(
    list(lustre@internals@vdom:attribute(RGN)),
    list(lustre@internals@vdom:element(RGN))
) -> lustre@internals@vdom:element(RGN).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 84).
-spec aside(
    list(lustre@internals@vdom:attribute(RGT)),
    list(lustre@internals@vdom:element(RGT))
) -> lustre@internals@vdom:element(RGT).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 92).
-spec footer(
    list(lustre@internals@vdom:attribute(RGZ)),
    list(lustre@internals@vdom:element(RGZ))
) -> lustre@internals@vdom:element(RGZ).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 100).
-spec header(
    list(lustre@internals@vdom:attribute(RHF)),
    list(lustre@internals@vdom:element(RHF))
) -> lustre@internals@vdom:element(RHF).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 108).
-spec h1(
    list(lustre@internals@vdom:attribute(RHL)),
    list(lustre@internals@vdom:element(RHL))
) -> lustre@internals@vdom:element(RHL).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 116).
-spec h2(
    list(lustre@internals@vdom:attribute(RHR)),
    list(lustre@internals@vdom:element(RHR))
) -> lustre@internals@vdom:element(RHR).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 124).
-spec h3(
    list(lustre@internals@vdom:attribute(RHX)),
    list(lustre@internals@vdom:element(RHX))
) -> lustre@internals@vdom:element(RHX).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 132).
-spec h4(
    list(lustre@internals@vdom:attribute(RID)),
    list(lustre@internals@vdom:element(RID))
) -> lustre@internals@vdom:element(RID).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 140).
-spec h5(
    list(lustre@internals@vdom:attribute(RIJ)),
    list(lustre@internals@vdom:element(RIJ))
) -> lustre@internals@vdom:element(RIJ).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 148).
-spec h6(
    list(lustre@internals@vdom:attribute(RIP)),
    list(lustre@internals@vdom:element(RIP))
) -> lustre@internals@vdom:element(RIP).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 156).
-spec hgroup(
    list(lustre@internals@vdom:attribute(RIV)),
    list(lustre@internals@vdom:element(RIV))
) -> lustre@internals@vdom:element(RIV).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 164).
-spec main(
    list(lustre@internals@vdom:attribute(RJB)),
    list(lustre@internals@vdom:element(RJB))
) -> lustre@internals@vdom:element(RJB).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 172).
-spec nav(
    list(lustre@internals@vdom:attribute(RJH)),
    list(lustre@internals@vdom:element(RJH))
) -> lustre@internals@vdom:element(RJH).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 180).
-spec section(
    list(lustre@internals@vdom:attribute(RJN)),
    list(lustre@internals@vdom:element(RJN))
) -> lustre@internals@vdom:element(RJN).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 188).
-spec search(
    list(lustre@internals@vdom:attribute(RJT)),
    list(lustre@internals@vdom:element(RJT))
) -> lustre@internals@vdom:element(RJT).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 198).
-spec blockquote(
    list(lustre@internals@vdom:attribute(RJZ)),
    list(lustre@internals@vdom:element(RJZ))
) -> lustre@internals@vdom:element(RJZ).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 206).
-spec dd(
    list(lustre@internals@vdom:attribute(RKF)),
    list(lustre@internals@vdom:element(RKF))
) -> lustre@internals@vdom:element(RKF).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 214).
-spec 'div'(
    list(lustre@internals@vdom:attribute(RKL)),
    list(lustre@internals@vdom:element(RKL))
) -> lustre@internals@vdom:element(RKL).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 222).
-spec dl(
    list(lustre@internals@vdom:attribute(RKR)),
    list(lustre@internals@vdom:element(RKR))
) -> lustre@internals@vdom:element(RKR).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 230).
-spec dt(
    list(lustre@internals@vdom:attribute(RKX)),
    list(lustre@internals@vdom:element(RKX))
) -> lustre@internals@vdom:element(RKX).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 238).
-spec figcaption(
    list(lustre@internals@vdom:attribute(RLD)),
    list(lustre@internals@vdom:element(RLD))
) -> lustre@internals@vdom:element(RLD).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 246).
-spec figure(
    list(lustre@internals@vdom:attribute(RLJ)),
    list(lustre@internals@vdom:element(RLJ))
) -> lustre@internals@vdom:element(RLJ).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 254).
-spec hr(list(lustre@internals@vdom:attribute(RLP))) -> lustre@internals@vdom:element(RLP).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 259).
-spec li(
    list(lustre@internals@vdom:attribute(RLT)),
    list(lustre@internals@vdom:element(RLT))
) -> lustre@internals@vdom:element(RLT).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 267).
-spec menu(
    list(lustre@internals@vdom:attribute(RLZ)),
    list(lustre@internals@vdom:element(RLZ))
) -> lustre@internals@vdom:element(RLZ).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 275).
-spec ol(
    list(lustre@internals@vdom:attribute(RMF)),
    list(lustre@internals@vdom:element(RMF))
) -> lustre@internals@vdom:element(RMF).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 283).
-spec p(
    list(lustre@internals@vdom:attribute(RML)),
    list(lustre@internals@vdom:element(RML))
) -> lustre@internals@vdom:element(RML).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 291).
-spec pre(
    list(lustre@internals@vdom:attribute(RMR)),
    list(lustre@internals@vdom:element(RMR))
) -> lustre@internals@vdom:element(RMR).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 299).
-spec ul(
    list(lustre@internals@vdom:attribute(RMX)),
    list(lustre@internals@vdom:element(RMX))
) -> lustre@internals@vdom:element(RMX).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 309).
-spec a(
    list(lustre@internals@vdom:attribute(RND)),
    list(lustre@internals@vdom:element(RND))
) -> lustre@internals@vdom:element(RND).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 317).
-spec abbr(
    list(lustre@internals@vdom:attribute(RNJ)),
    list(lustre@internals@vdom:element(RNJ))
) -> lustre@internals@vdom:element(RNJ).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 325).
-spec b(
    list(lustre@internals@vdom:attribute(RNP)),
    list(lustre@internals@vdom:element(RNP))
) -> lustre@internals@vdom:element(RNP).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 333).
-spec bdi(
    list(lustre@internals@vdom:attribute(RNV)),
    list(lustre@internals@vdom:element(RNV))
) -> lustre@internals@vdom:element(RNV).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 341).
-spec bdo(
    list(lustre@internals@vdom:attribute(ROB)),
    list(lustre@internals@vdom:element(ROB))
) -> lustre@internals@vdom:element(ROB).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 349).
-spec br(list(lustre@internals@vdom:attribute(ROH))) -> lustre@internals@vdom:element(ROH).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 354).
-spec cite(
    list(lustre@internals@vdom:attribute(ROL)),
    list(lustre@internals@vdom:element(ROL))
) -> lustre@internals@vdom:element(ROL).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 362).
-spec code(
    list(lustre@internals@vdom:attribute(ROR)),
    list(lustre@internals@vdom:element(ROR))
) -> lustre@internals@vdom:element(ROR).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 370).
-spec data(
    list(lustre@internals@vdom:attribute(ROX)),
    list(lustre@internals@vdom:element(ROX))
) -> lustre@internals@vdom:element(ROX).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 378).
-spec dfn(
    list(lustre@internals@vdom:attribute(RPD)),
    list(lustre@internals@vdom:element(RPD))
) -> lustre@internals@vdom:element(RPD).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 386).
-spec em(
    list(lustre@internals@vdom:attribute(RPJ)),
    list(lustre@internals@vdom:element(RPJ))
) -> lustre@internals@vdom:element(RPJ).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 394).
-spec i(
    list(lustre@internals@vdom:attribute(RPP)),
    list(lustre@internals@vdom:element(RPP))
) -> lustre@internals@vdom:element(RPP).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 402).
-spec kbd(
    list(lustre@internals@vdom:attribute(RPV)),
    list(lustre@internals@vdom:element(RPV))
) -> lustre@internals@vdom:element(RPV).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 410).
-spec mark(
    list(lustre@internals@vdom:attribute(RQB)),
    list(lustre@internals@vdom:element(RQB))
) -> lustre@internals@vdom:element(RQB).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 418).
-spec q(
    list(lustre@internals@vdom:attribute(RQH)),
    list(lustre@internals@vdom:element(RQH))
) -> lustre@internals@vdom:element(RQH).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 426).
-spec rp(
    list(lustre@internals@vdom:attribute(RQN)),
    list(lustre@internals@vdom:element(RQN))
) -> lustre@internals@vdom:element(RQN).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 434).
-spec rt(
    list(lustre@internals@vdom:attribute(RQT)),
    list(lustre@internals@vdom:element(RQT))
) -> lustre@internals@vdom:element(RQT).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 442).
-spec ruby(
    list(lustre@internals@vdom:attribute(RQZ)),
    list(lustre@internals@vdom:element(RQZ))
) -> lustre@internals@vdom:element(RQZ).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 450).
-spec s(
    list(lustre@internals@vdom:attribute(RRF)),
    list(lustre@internals@vdom:element(RRF))
) -> lustre@internals@vdom:element(RRF).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 458).
-spec samp(
    list(lustre@internals@vdom:attribute(RRL)),
    list(lustre@internals@vdom:element(RRL))
) -> lustre@internals@vdom:element(RRL).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 466).
-spec small(
    list(lustre@internals@vdom:attribute(RRR)),
    list(lustre@internals@vdom:element(RRR))
) -> lustre@internals@vdom:element(RRR).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 474).
-spec span(
    list(lustre@internals@vdom:attribute(RRX)),
    list(lustre@internals@vdom:element(RRX))
) -> lustre@internals@vdom:element(RRX).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 482).
-spec strong(
    list(lustre@internals@vdom:attribute(RSD)),
    list(lustre@internals@vdom:element(RSD))
) -> lustre@internals@vdom:element(RSD).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 490).
-spec sub(
    list(lustre@internals@vdom:attribute(RSJ)),
    list(lustre@internals@vdom:element(RSJ))
) -> lustre@internals@vdom:element(RSJ).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 498).
-spec sup(
    list(lustre@internals@vdom:attribute(RSP)),
    list(lustre@internals@vdom:element(RSP))
) -> lustre@internals@vdom:element(RSP).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 506).
-spec time(
    list(lustre@internals@vdom:attribute(RSV)),
    list(lustre@internals@vdom:element(RSV))
) -> lustre@internals@vdom:element(RSV).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 514).
-spec u(
    list(lustre@internals@vdom:attribute(RTB)),
    list(lustre@internals@vdom:element(RTB))
) -> lustre@internals@vdom:element(RTB).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 522).
-spec var(
    list(lustre@internals@vdom:attribute(RTH)),
    list(lustre@internals@vdom:element(RTH))
) -> lustre@internals@vdom:element(RTH).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 530).
-spec wbr(list(lustre@internals@vdom:attribute(RTN))) -> lustre@internals@vdom:element(RTN).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 537).
-spec area(list(lustre@internals@vdom:attribute(RTR))) -> lustre@internals@vdom:element(RTR).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 542).
-spec audio(
    list(lustre@internals@vdom:attribute(RTV)),
    list(lustre@internals@vdom:element(RTV))
) -> lustre@internals@vdom:element(RTV).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 550).
-spec img(list(lustre@internals@vdom:attribute(RUB))) -> lustre@internals@vdom:element(RUB).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 556).
-spec map(
    list(lustre@internals@vdom:attribute(RUF)),
    list(lustre@internals@vdom:element(RUF))
) -> lustre@internals@vdom:element(RUF).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 564).
-spec track(list(lustre@internals@vdom:attribute(RUL))) -> lustre@internals@vdom:element(RUL).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 569).
-spec video(
    list(lustre@internals@vdom:attribute(RUP)),
    list(lustre@internals@vdom:element(RUP))
) -> lustre@internals@vdom:element(RUP).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 579).
-spec embed(list(lustre@internals@vdom:attribute(RUV))) -> lustre@internals@vdom:element(RUV).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 584).
-spec iframe(list(lustre@internals@vdom:attribute(RUZ))) -> lustre@internals@vdom:element(RUZ).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 589).
-spec object(list(lustre@internals@vdom:attribute(RVD))) -> lustre@internals@vdom:element(RVD).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 594).
-spec picture(
    list(lustre@internals@vdom:attribute(RVH)),
    list(lustre@internals@vdom:element(RVH))
) -> lustre@internals@vdom:element(RVH).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 602).
-spec portal(list(lustre@internals@vdom:attribute(RVN))) -> lustre@internals@vdom:element(RVN).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 607).
-spec source(list(lustre@internals@vdom:attribute(RVR))) -> lustre@internals@vdom:element(RVR).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 614).
-spec svg(
    list(lustre@internals@vdom:attribute(RVV)),
    list(lustre@internals@vdom:element(RVV))
) -> lustre@internals@vdom:element(RVV).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 622).
-spec math(
    list(lustre@internals@vdom:attribute(RWB)),
    list(lustre@internals@vdom:element(RWB))
) -> lustre@internals@vdom:element(RWB).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 632).
-spec canvas(list(lustre@internals@vdom:attribute(RWH))) -> lustre@internals@vdom:element(RWH).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 637).
-spec noscript(
    list(lustre@internals@vdom:attribute(RWL)),
    list(lustre@internals@vdom:element(RWL))
) -> lustre@internals@vdom:element(RWL).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 645).
-spec script(list(lustre@internals@vdom:attribute(RWR)), binary()) -> lustre@internals@vdom:element(RWR).
script(Attrs, Js) ->
    lustre@element:element(<<"script"/utf8>>, Attrs, [text(Js)]).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 652).
-spec del(
    list(lustre@internals@vdom:attribute(RWV)),
    list(lustre@internals@vdom:element(RWV))
) -> lustre@internals@vdom:element(RWV).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 660).
-spec ins(
    list(lustre@internals@vdom:attribute(RXB)),
    list(lustre@internals@vdom:element(RXB))
) -> lustre@internals@vdom:element(RXB).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 670).
-spec caption(
    list(lustre@internals@vdom:attribute(RXH)),
    list(lustre@internals@vdom:element(RXH))
) -> lustre@internals@vdom:element(RXH).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 678).
-spec col(list(lustre@internals@vdom:attribute(RXN))) -> lustre@internals@vdom:element(RXN).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 683).
-spec colgroup(
    list(lustre@internals@vdom:attribute(RXR)),
    list(lustre@internals@vdom:element(RXR))
) -> lustre@internals@vdom:element(RXR).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 691).
-spec table(
    list(lustre@internals@vdom:attribute(RXX)),
    list(lustre@internals@vdom:element(RXX))
) -> lustre@internals@vdom:element(RXX).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 699).
-spec tbody(
    list(lustre@internals@vdom:attribute(RYD)),
    list(lustre@internals@vdom:element(RYD))
) -> lustre@internals@vdom:element(RYD).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 707).
-spec td(
    list(lustre@internals@vdom:attribute(RYJ)),
    list(lustre@internals@vdom:element(RYJ))
) -> lustre@internals@vdom:element(RYJ).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 715).
-spec tfoot(
    list(lustre@internals@vdom:attribute(RYP)),
    list(lustre@internals@vdom:element(RYP))
) -> lustre@internals@vdom:element(RYP).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 723).
-spec th(
    list(lustre@internals@vdom:attribute(RYV)),
    list(lustre@internals@vdom:element(RYV))
) -> lustre@internals@vdom:element(RYV).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 731).
-spec thead(
    list(lustre@internals@vdom:attribute(RZB)),
    list(lustre@internals@vdom:element(RZB))
) -> lustre@internals@vdom:element(RZB).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 739).
-spec tr(
    list(lustre@internals@vdom:attribute(RZH)),
    list(lustre@internals@vdom:element(RZH))
) -> lustre@internals@vdom:element(RZH).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 749).
-spec button(
    list(lustre@internals@vdom:attribute(RZN)),
    list(lustre@internals@vdom:element(RZN))
) -> lustre@internals@vdom:element(RZN).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 757).
-spec datalist(
    list(lustre@internals@vdom:attribute(RZT)),
    list(lustre@internals@vdom:element(RZT))
) -> lustre@internals@vdom:element(RZT).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 765).
-spec fieldset(
    list(lustre@internals@vdom:attribute(RZZ)),
    list(lustre@internals@vdom:element(RZZ))
) -> lustre@internals@vdom:element(RZZ).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 773).
-spec form(
    list(lustre@internals@vdom:attribute(SAF)),
    list(lustre@internals@vdom:element(SAF))
) -> lustre@internals@vdom:element(SAF).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 781).
-spec input(list(lustre@internals@vdom:attribute(SAL))) -> lustre@internals@vdom:element(SAL).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 786).
-spec label(
    list(lustre@internals@vdom:attribute(SAP)),
    list(lustre@internals@vdom:element(SAP))
) -> lustre@internals@vdom:element(SAP).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 794).
-spec legend(
    list(lustre@internals@vdom:attribute(SAV)),
    list(lustre@internals@vdom:element(SAV))
) -> lustre@internals@vdom:element(SAV).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 802).
-spec meter(
    list(lustre@internals@vdom:attribute(SBB)),
    list(lustre@internals@vdom:element(SBB))
) -> lustre@internals@vdom:element(SBB).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 810).
-spec optgroup(
    list(lustre@internals@vdom:attribute(SBH)),
    list(lustre@internals@vdom:element(SBH))
) -> lustre@internals@vdom:element(SBH).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 818).
-spec option(list(lustre@internals@vdom:attribute(SBN)), binary()) -> lustre@internals@vdom:element(SBN).
option(Attrs, Label) ->
    lustre@element:element(
        <<"option"/utf8>>,
        Attrs,
        [lustre@element:text(Label)]
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 823).
-spec output(
    list(lustre@internals@vdom:attribute(SBR)),
    list(lustre@internals@vdom:element(SBR))
) -> lustre@internals@vdom:element(SBR).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 831).
-spec progress(
    list(lustre@internals@vdom:attribute(SBX)),
    list(lustre@internals@vdom:element(SBX))
) -> lustre@internals@vdom:element(SBX).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 839).
-spec select(
    list(lustre@internals@vdom:attribute(SCD)),
    list(lustre@internals@vdom:element(SCD))
) -> lustre@internals@vdom:element(SCD).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 847).
-spec textarea(list(lustre@internals@vdom:attribute(SCJ)), binary()) -> lustre@internals@vdom:element(SCJ).
textarea(Attrs, Content) ->
    lustre@element:element(
        <<"textarea"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 854).
-spec details(
    list(lustre@internals@vdom:attribute(SCN)),
    list(lustre@internals@vdom:element(SCN))
) -> lustre@internals@vdom:element(SCN).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 862).
-spec dialog(
    list(lustre@internals@vdom:attribute(SCT)),
    list(lustre@internals@vdom:element(SCT))
) -> lustre@internals@vdom:element(SCT).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 870).
-spec summary(
    list(lustre@internals@vdom:attribute(SCZ)),
    list(lustre@internals@vdom:element(SCZ))
) -> lustre@internals@vdom:element(SCZ).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 880).
-spec slot(list(lustre@internals@vdom:attribute(SDF))) -> lustre@internals@vdom:element(SDF).
slot(Attrs) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/element/html.gleam", 885).
-spec template(
    list(lustre@internals@vdom:attribute(SDJ)),
    list(lustre@internals@vdom:element(SDJ))
) -> lustre@internals@vdom:element(SDJ).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
