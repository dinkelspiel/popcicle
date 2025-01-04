-module(lustre@server_component).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([component/1, script/0, route/1, data/1, include/1, subscribe/2, unsubscribe/1, emit/2, select/1, set_selector/1, decode_action/1, encode_patch/1]).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 101).
-spec component(list(lustre@internals@vdom:attribute(TFD))) -> lustre@internals@vdom:element(TFD).
component(Attrs) ->
    lustre@element:element(<<"lustre-server-component"/utf8>>, Attrs, []).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 107).
-spec script() -> lustre@internals@vdom:element(any()).
script() ->
    lustre@element:element(
        <<"script"/utf8>>,
        [lustre@attribute:attribute(<<"type"/utf8>>, <<"module"/utf8>>)],
        [lustre@element:text(
                <<"globalThis.customElements&&!globalThis.customElements.get(\"lustre-fragment\")&&globalThis.customElements.define(\"lustre-fragment\",class extends HTMLElement{constructor(){super()}});function v(t,e,r){let s,i=[{prev:t,next:e,parent:t.parentNode}];for(;i.length;){let{prev:o,next:n,parent:l}=i.pop();for(;n.subtree!==void 0;)n=n.subtree();if(n.content!==void 0)if(o)if(o.nodeType===Node.TEXT_NODE)o.textContent!==n.content&&(o.textContent=n.content),s??=o;else{let a=document.createTextNode(n.content);l.replaceChild(a,o),s??=a}else{let a=document.createTextNode(n.content);l.appendChild(a),s??=a}else if(n.tag!==void 0){let a=$({prev:o,next:n,dispatch:r,stack:i});o?o!==a&&l.replaceChild(a,o):l.appendChild(a),s??=a}}return s}function _(t,e,r,s=0){let i=t.parentNode;for(let o of e[0]){let n=o[0].split(\"-\"),l=o[1],a=E(i,n,s),c;if(a!==null&&a!==i)c=v(a,l,r);else{let p=E(i,n.slice(0,-1),s),g=document.createTextNode(\"\");p.appendChild(g),c=v(g,l,r)}n===\"0\"&&(t=c)}for(let o of e[1]){let n=o[0].split(\"-\");E(i,n,s).remove()}for(let o of e[2]){let n=o[0].split(\"-\"),l=o[1],a=E(i,n,s),c=x.get(a),p=[];for(let g of l[0]){let d=g[0],b=g[1];if(d.startsWith(\"data-lustre-on-\")){let m=d.slice(15),S=r(F);c.has(m)||a.addEventListener(m,w),c.set(m,S),a.setAttribute(d,b)}else(d.startsWith(\"delegate:data-\")||d.startsWith(\"delegate:aria-\"))&&a instanceof HTMLSlotElement?p.push([d.slice(10),b]):(a.setAttribute(d,b),(d===\"value\"||d===\"selected\")&&(a[d]=b));if(p.length>0)for(let m of a.assignedElements())for(let[S,A]of p)m[S]=A}for(let g of l[1])if(g.startsWith(\"data-lustre-on-\")){let d=g.slice(15);a.removeEventListener(d,w),c.delete(d)}else a.removeAttribute(g)}return t}function $({prev:t,next:e,dispatch:r,stack:s}){let i=e.namespace||\"http://www.w3.org/1999/xhtml\",o=t&&t.nodeType===Node.ELEMENT_NODE&&t.localName===e.tag&&t.namespaceURI===(e.namespace||\"http://www.w3.org/1999/xhtml\"),n=o?t:i?document.createElementNS(i,e.tag):document.createElement(e.tag),l;if(x.has(n))l=x.get(n);else{let u=new Map;x.set(n,u),l=u}let a=o?new Set(l.keys()):null,c=o?new Set(Array.from(t.attributes,u=>u.name)):null,p=null,g=null,d=null;if(o&&e.tag===\"textarea\"){let u=e.children[Symbol.iterator]().next().value?.content;u!==void 0&&(n.value=u)}let b=[];for(let u of e.attrs){let f=u[0],h=u[1];if(u.as_property)n[f]!==h&&(n[f]=h),o&&c.delete(f);else if(f.startsWith(\"on\")){let y=f.slice(2),T=r(h,y===\"input\");l.has(y)||n.addEventListener(y,w),l.set(y,T),o&&a.delete(y)}else if(f.startsWith(\"data-lustre-on-\")){let y=f.slice(15),T=r(F);l.has(y)||n.addEventListener(y,w),l.set(y,T),n.setAttribute(f,h),o&&(a.delete(y),c.delete(f))}else f.startsWith(\"delegate:data-\")||f.startsWith(\"delegate:aria-\")?(n.setAttribute(f,h),b.push([f.slice(10),h])):f===\"class\"?p=p===null?h:p+\" \"+h:f===\"style\"?g=g===null?h:g+h:f===\"dangerous-unescaped-html\"?d=h:(n.getAttribute(f)!==h&&n.setAttribute(f,h),(f===\"value\"||f===\"selected\")&&(n[f]=h),o&&c.delete(f))}if(p!==null&&(n.setAttribute(\"class\",p),o&&c.delete(\"class\")),g!==null&&(n.setAttribute(\"style\",g),o&&c.delete(\"style\")),o){for(let u of c)n.removeAttribute(u);for(let u of a)l.delete(u),n.removeEventListener(u,w)}if(e.tag===\"slot\"&&window.queueMicrotask(()=>{for(let u of n.assignedElements())for(let[f,h]of b)u.hasAttribute(f)||u.setAttribute(f,h)}),e.key!==void 0&&e.key!==\"\")n.setAttribute(\"data-lustre-key\",e.key);else if(d!==null)return n.innerHTML=d,n;let m=n.firstChild,S=null,A=null,C=null,N=k(e).next().value;if(o&&N!==void 0&&N.key!==void 0&&N.key!==\"\"){S=new Set,A=R(t),C=R(e);for(let u of k(e))m=j(m,u,n,s,C,A,S)}else for(let u of k(e))s.unshift({prev:m,next:u,parent:n}),m=m?.nextSibling;for(;m;){let u=m.nextSibling;n.removeChild(m),m=u}return n}var x=new WeakMap;function w(t){let e=t.currentTarget;if(!x.has(e)){e.removeEventListener(t.type,w);return}let r=x.get(e);if(!r.has(t.type)){e.removeEventListener(t.type,w);return}r.get(t.type)(t)}function F(t){let e=t.currentTarget,r=e.getAttribute(`data-lustre-on-${t.type}`),s=JSON.parse(e.getAttribute(\"data-lustre-data\")||\"{}\"),i=JSON.parse(e.getAttribute(\"data-lustre-include\")||\"[]\");switch(t.type){case\"input\":case\"change\":i.push(\"target.value\");break}return{tag:r,data:i.reduce((o,n)=>{let l=n.split(\".\");for(let a=0,c=o,p=t;a<l.length;a++)a===l.length-1?c[l[a]]=p[l[a]]:(c[l[a]]??={},p=p[l[a]],c=c[l[a]]);return o},{data:s})}}function R(t){let e=new Map;if(t)for(let r of k(t)){let s=r?.key||r?.getAttribute?.(\"data-lustre-key\");s&&e.set(s,r)}return e}function E(t,e,r){let s,i,o=t,n=!0;for(;[s,...i]=e,s!==void 0;)o=o.childNodes.item(n?s+r:s),n=!1,e=i;return o}function j(t,e,r,s,i,o,n){for(;t&&!i.has(t.getAttribute(\"data-lustre-key\"));){let a=t.nextSibling;r.removeChild(t),t=a}if(o.size===0)return s.unshift({prev:t,next:e,parent:r}),t=t?.nextSibling,t;if(n.has(e.key))return console.warn(`Duplicate key found in Lustre vnode: ${e.key}`),s.unshift({prev:null,next:e,parent:r}),t;n.add(e.key);let l=o.get(e.key);if(!l&&!t)return s.unshift({prev:null,next:e,parent:r}),t;if(!l&&t!==null){let a=document.createTextNode(\"\");return r.insertBefore(a,t),s.unshift({prev:a,next:e,parent:r}),t}return!l||l===t?(s.unshift({prev:t,next:e,parent:r}),t=t?.nextSibling,t):(r.insertBefore(l,t),s.unshift({prev:l,next:e,parent:r}),t)}function*k(t){for(let e of t.children)yield*M(e)}function*M(t){t.subtree!==void 0?yield*M(t.subtree()):yield t}function W(t,e){let r=[t,e];for(;r.length;){let s=r.pop(),i=r.pop();if(s===i)continue;if(!q(s)||!q(i)||!G(s,i)||B(s,i)||U(s,i)||z(s,i)||I(s,i)||V(s,i)||K(s,i))return!1;let n=Object.getPrototypeOf(s);if(n!==null&&typeof n.equals==\"function\")try{if(s.equals(i))continue;return!1}catch{}let[l,a]=H(s);for(let c of l(s))r.push(a(s,c),a(i,c))}return!0}function H(t){if(t instanceof Map)return[e=>e.keys(),(e,r)=>e.get(r)];{let e=t instanceof globalThis.Error?[\"message\"]:[];return[r=>[...e,...Object.keys(r)],(r,s)=>r[s]]}}function B(t,e){return t instanceof Date&&(t>e||t<e)}function U(t,e){return t.buffer instanceof ArrayBuffer&&t.BYTES_PER_ELEMENT&&!(t.byteLength===e.byteLength&&t.every((r,s)=>r===e[s]))}function z(t,e){return Array.isArray(t)&&t.length!==e.length}function I(t,e){return t instanceof Map&&t.size!==e.size}function V(t,e){return t instanceof Set&&(t.size!=e.size||[...t].some(r=>!e.has(r)))}function K(t,e){return t instanceof RegExp&&(t.source!==e.source||t.flags!==e.flags)}function q(t){return typeof t==\"object\"&&t!==null}function G(t,e){return typeof t!=\"object\"&&typeof e!=\"object\"&&(!t||!e)||[Promise,WeakSet,WeakMap,Function].some(s=>t instanceof s)?!1:t.constructor===e.constructor}var O=class extends HTMLElement{static get observedAttributes(){return[\"route\"]}constructor(){super(),this.attachShadow({mode:\"open\"}),this.#n=new MutationObserver(e=>{let r=[];for(let s of e)if(s.type===\"attributes\"){let{attributeName:i}=s,o=this.getAttribute(i);this[i]=o}r.length&&this.#t?.send(JSON.stringify([5,r]))})}connectedCallback(){this.#n.observe(this,{attributes:!0,attributeOldValue:!0}),this.#l().finally(()=>this.#r=!0)}attributeChangedCallback(e,r,s){switch(e){case\"route\":if(!s)this.#t?.close(),this.#t=null;else if(r!==s){let i=this.getAttribute(\"id\"),o=s+(i?`?id=${i}`:\"\"),n=window.location.protocol===\"https:\"?\"wss\":\"ws\";this.#s(`${n}://${window.location.host}${o}`)}}}messageReceivedCallback({data:e}){let[r,...s]=JSON.parse(e);switch(r){case 0:return this.#i(s);case 1:return this.#a(s);case 2:return this.#o(s)}}disconnectedCallback(){this.#t?.close()}#n;#t;#r=!1;#e=[];#o([e,r]){let s=[];for(let n of e)n in this?s.push([n,this[n]]):this.hasAttribute(n)&&s.push([n,this.getAttribute(n)]),Object.defineProperty(this,n,{get(){return this[`__mirrored__${n}`]},set(l){let a=this[`__mirrored__${n}`];W(a,l)||(this[`__mirrored__${n}`]=l,this.#t?.send(JSON.stringify([5,[[n,l]]])))}});this.#n.observe(this,{attributeFilter:e,attributeOldValue:!0,attributes:!0,characterData:!1,characterDataOldValue:!1,childList:!1,subtree:!1});let i=this.shadowRoot.childNodes[this.#e.length]??this.shadowRoot.appendChild(document.createTextNode(\"\"));v(i,r,n=>l=>{let a=JSON.parse(this.getAttribute(\"data-lustre-data\")||\"{}\"),c=n(l);c.data=J(a,c.data),this.#t?.send(JSON.stringify([4,c.tag,c.data]))}),s.length&&this.#t?.send(JSON.stringify([5,s]))}#s(e=this.#t.url){this.#t?.close(),this.#t=new WebSocket(e),this.#t.addEventListener(\"message\",r=>this.messageReceivedCallback(r)),this.#t.addEventListener(\"close\",()=>{setTimeout(()=>{this.#t.readyState===WebSocket.CLOSED&&this.#s()},1e3)})}#i([e]){let r=this.shadowRoot.childNodes[this.#e.length-1]??this.shadowRoot.appendChild(document.createTextNode(\"\"));_(r,e,i=>o=>{let n=i(o);this.#t?.send(JSON.stringify([4,n.tag,n.data]))},this.#e.length)}#a([e,r]){this.dispatchEvent(new CustomEvent(e,{detail:r}))}async#l(){let e=[];for(let s of document.querySelectorAll(\"link[rel=stylesheet]\"))s.sheet||e.push(new Promise((i,o)=>{s.addEventListener(\"load\",i),s.addEventListener(\"error\",o)}));for(await Promise.allSettled(e);this.#e.length;)this.#e.shift().remove(),this.shadowRoot.firstChild.remove();this.shadowRoot.adoptedStyleSheets=this.getRootNode().adoptedStyleSheets;let r=[];for(let s of document.styleSheets)try{this.shadowRoot.adoptedStyleSheets.push(s)}catch{try{let i=new CSSStyleSheet;for(let o of s.cssRules)i.insertRule(o.cssText,i.cssRules.length);this.shadowRoot.adoptedStyleSheets.push(i)}catch{let i=s.ownerNode.cloneNode();this.shadowRoot.prepend(i),this.#e.push(i),r.push(new Promise((o,n)=>{i.onload=o,i.onerror=n}))}}return Promise.allSettled(r)}};window.customElements.define(\"lustre-server-component\",O);var J=(t,e)=>{for(let r in e)e[r]instanceof Object&&Object.assign(e[r],J(t[r],e[r]));return Object.assign(t||{},e),t};export{O as LustreServerComponent};"/utf8>>
            )]
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 123).
-spec route(binary()) -> lustre@internals@vdom:attribute(any()).
route(Path) ->
    lustre@attribute:attribute(<<"route"/utf8>>, Path).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 134).
-spec data(gleam@json:json()) -> lustre@internals@vdom:attribute(any()).
data(Json) ->
    _pipe = Json,
    _pipe@1 = gleam@json:to_string(_pipe),
    lustre@attribute:attribute(<<"data-lustre-data"/utf8>>, _pipe@1).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 170).
-spec include(list(binary())) -> lustre@internals@vdom:attribute(any()).
include(Properties) ->
    _pipe = Properties,
    _pipe@1 = gleam@json:array(_pipe, fun gleam@json:string/1),
    _pipe@2 = gleam@json:to_string(_pipe@1),
    lustre@attribute:attribute(<<"data-lustre-include"/utf8>>, _pipe@2).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 182).
-spec subscribe(binary(), fun((lustre@internals@patch:patch(TFQ)) -> nil)) -> lustre@internals@runtime:action(TFQ, lustre:server_component()).
subscribe(Id, Renderer) ->
    {subscribe, Id, Renderer}.

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 192).
-spec unsubscribe(binary()) -> lustre@internals@runtime:action(any(), lustre:server_component()).
unsubscribe(Id) ->
    {unsubscribe, Id}.

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 206).
-spec emit(binary(), gleam@json:json()) -> lustre@effect:effect(any()).
emit(Event, Data) ->
    lustre@effect:event(Event, Data).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 236).
-spec do_select(
    fun((fun((TGE) -> nil), gleam@erlang@process:subject(any())) -> gleam@erlang@process:selector(TGE))
) -> lustre@effect:effect(TGE).
do_select(Sel) ->
    lustre@effect:custom(
        fun(Dispatch, _, Select, _) ->
            Self = gleam@erlang@process:new_subject(),
            Selector = Sel(Dispatch, Self),
            Select(Selector)
        end
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 229).
-spec select(
    fun((fun((TFZ) -> nil), gleam@erlang@process:subject(any())) -> gleam@erlang@process:selector(TFZ))
) -> lustre@effect:effect(TFZ).
select(Sel) ->
    do_select(Sel).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 257).
-spec set_selector(
    gleam@erlang@process:selector(lustre@internals@runtime:action(any(), TGK))
) -> lustre@effect:effect(TGK).
set_selector(_) ->
    lustre@effect:from(
        fun(_) ->
            _pipe = <<"
It looks like you're trying to use `set_selector` in a server component. The
implementation of this effect is broken in ways that cannot be fixed without
changing the API. Please take a look at `select` instead!
  "/utf8>>,
            _pipe@1 = gleam@string:trim(_pipe),
            gleam@io:println_error(_pipe@1),
            nil
        end
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 283).
-spec decode_event(gleam@dynamic:dynamic_()) -> {ok,
        lustre@internals@runtime:action(any(), any())} |
    {error, list(gleam@dynamic:decode_error())}.
decode_event(Dyn) ->
    gleam@result:'try'(
        (gleam@dynamic:tuple3(
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:dynamic/1,
            fun gleam@dynamic:dynamic/1
        ))(Dyn),
        fun(_use0) ->
            {Kind, Name, Data} = _use0,
            gleam@bool:guard(
                Kind /= 4,
                {error,
                    [{decode_error,
                            gleam@int:to_string(4),
                            gleam@int:to_string(Kind),
                            [<<"0"/utf8>>]}]},
                fun() ->
                    gleam@result:'try'(
                        gleam@dynamic:string(Name),
                        fun(Name@1) -> {ok, {event, Name@1, Data}} end
                    )
                end
            )
        end
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 321).
-spec decode_attr(gleam@dynamic:dynamic_()) -> {ok,
        {binary(), gleam@dynamic:dynamic_()}} |
    {error, list(gleam@dynamic:decode_error())}.
decode_attr(Dyn) ->
    (gleam@dynamic:tuple2(
        fun gleam@dynamic:string/1,
        fun gleam@dynamic:dynamic/1
    ))(Dyn).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 304).
-spec decode_attrs(gleam@dynamic:dynamic_()) -> {ok,
        lustre@internals@runtime:action(any(), any())} |
    {error, list(gleam@dynamic:decode_error())}.
decode_attrs(Dyn) ->
    gleam@result:'try'(
        (gleam@dynamic:tuple2(
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:dynamic/1
        ))(Dyn),
        fun(_use0) ->
            {Kind, Attrs} = _use0,
            gleam@bool:guard(
                Kind /= 5,
                {error,
                    [{decode_error,
                            gleam@int:to_string(5),
                            gleam@int:to_string(Kind),
                            [<<"0"/utf8>>]}]},
                fun() ->
                    gleam@result:'try'(
                        (gleam@dynamic:list(fun decode_attr/1))(Attrs),
                        fun(Attrs@1) -> {ok, {attrs, Attrs@1}} end
                    )
                end
            )
        end
    ).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 277).
-spec decode_action(gleam@dynamic:dynamic_()) -> {ok,
        lustre@internals@runtime:action(any(), lustre:server_component())} |
    {error, list(gleam@dynamic:decode_error())}.
decode_action(Dyn) ->
    (gleam@dynamic:any([fun decode_event/1, fun decode_attrs/1]))(Dyn).

-file("/home/runner/work/lustre/lustre/src/lustre/server_component.gleam", 331).
-spec encode_patch(lustre@internals@patch:patch(any())) -> gleam@json:json().
encode_patch(Patch) ->
    lustre@internals@patch:patch_to_json(Patch).
