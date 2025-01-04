// build/dev/javascript/prelude.mjs
var CustomType = class {
  withFields(fields) {
    let properties = Object.keys(this).map(
      (label2) => label2 in fields ? fields[label2] : this[label2]
    );
    return new this.constructor(...properties);
  }
};
var List = class {
  static fromArray(array3, tail) {
    let t = tail || new Empty();
    for (let i = array3.length - 1; i >= 0; --i) {
      t = new NonEmpty(array3[i], t);
    }
    return t;
  }
  [Symbol.iterator]() {
    return new ListIterator(this);
  }
  toArray() {
    return [...this];
  }
  // @internal
  atLeastLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return true;
      desired--;
    }
    return desired <= 0;
  }
  // @internal
  hasLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return false;
      desired--;
    }
    return desired === 0;
  }
  // @internal
  countLength() {
    let length3 = 0;
    for (let _ of this)
      length3++;
    return length3;
  }
};
function prepend(element2, tail) {
  return new NonEmpty(element2, tail);
}
function toList(elements2, tail) {
  return List.fromArray(elements2, tail);
}
var ListIterator = class {
  #current;
  constructor(current) {
    this.#current = current;
  }
  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
};
var Empty = class extends List {
};
var NonEmpty = class extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
};
var BitArray = class _BitArray {
  constructor(buffer) {
    if (!(buffer instanceof Uint8Array)) {
      throw "BitArray can only be constructed from a Uint8Array";
    }
    this.buffer = buffer;
  }
  // @internal
  get length() {
    return this.buffer.length;
  }
  // @internal
  byteAt(index3) {
    return this.buffer[index3];
  }
  // @internal
  floatFromSlice(start3, end, isBigEndian) {
    return byteArrayToFloat(this.buffer, start3, end, isBigEndian);
  }
  // @internal
  intFromSlice(start3, end, isBigEndian, isSigned) {
    return byteArrayToInt(this.buffer, start3, end, isBigEndian, isSigned);
  }
  // @internal
  binaryFromSlice(start3, end) {
    return new _BitArray(this.buffer.slice(start3, end));
  }
  // @internal
  sliceAfter(index3) {
    return new _BitArray(this.buffer.slice(index3));
  }
};
function byteArrayToInt(byteArray, start3, end, isBigEndian, isSigned) {
  const byteSize = end - start3;
  if (byteSize <= 6) {
    let value2 = 0;
    if (isBigEndian) {
      for (let i = start3; i < end; i++) {
        value2 = value2 * 256 + byteArray[i];
      }
    } else {
      for (let i = end - 1; i >= start3; i--) {
        value2 = value2 * 256 + byteArray[i];
      }
    }
    if (isSigned) {
      const highBit = 2 ** (byteSize * 8 - 1);
      if (value2 >= highBit) {
        value2 -= highBit * 2;
      }
    }
    return value2;
  } else {
    let value2 = 0n;
    if (isBigEndian) {
      for (let i = start3; i < end; i++) {
        value2 = (value2 << 8n) + BigInt(byteArray[i]);
      }
    } else {
      for (let i = end - 1; i >= start3; i--) {
        value2 = (value2 << 8n) + BigInt(byteArray[i]);
      }
    }
    if (isSigned) {
      const highBit = 1n << BigInt(byteSize * 8 - 1);
      if (value2 >= highBit) {
        value2 -= highBit * 2n;
      }
    }
    return Number(value2);
  }
}
function byteArrayToFloat(byteArray, start3, end, isBigEndian) {
  const view2 = new DataView(byteArray.buffer);
  const byteSize = end - start3;
  if (byteSize === 8) {
    return view2.getFloat64(start3, !isBigEndian);
  } else if (byteSize === 4) {
    return view2.getFloat32(start3, !isBigEndian);
  } else {
    const msg = `Sized floats must be 32-bit or 64-bit on JavaScript, got size of ${byteSize * 8} bits`;
    throw new globalThis.Error(msg);
  }
}
var Result = class _Result extends CustomType {
  // @internal
  static isResult(data) {
    return data instanceof _Result;
  }
};
var Ok = class extends Result {
  constructor(value2) {
    super();
    this[0] = value2;
  }
  // @internal
  isOk() {
    return true;
  }
};
var Error = class extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
  // @internal
  isOk() {
    return false;
  }
};
function isEqual(x, y) {
  let values2 = [x, y];
  while (values2.length) {
    let a2 = values2.pop();
    let b = values2.pop();
    if (a2 === b)
      continue;
    if (!isObject(a2) || !isObject(b))
      return false;
    let unequal = !structurallyCompatibleObjects(a2, b) || unequalDates(a2, b) || unequalBuffers(a2, b) || unequalArrays(a2, b) || unequalMaps(a2, b) || unequalSets(a2, b) || unequalRegExps(a2, b);
    if (unequal)
      return false;
    const proto = Object.getPrototypeOf(a2);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a2.equals(b))
          continue;
        else
          return false;
      } catch {
      }
    }
    let [keys2, get] = getters(a2);
    for (let k of keys2(a2)) {
      values2.push(get(a2, k), get(b, k));
    }
  }
  return true;
}
function getters(object3) {
  if (object3 instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object3 instanceof globalThis.Error ? ["message"] : [];
    return [(x) => [...extra, ...Object.keys(x)], (x, y) => x[y]];
  }
}
function unequalDates(a2, b) {
  return a2 instanceof Date && (a2 > b || a2 < b);
}
function unequalBuffers(a2, b) {
  return a2.buffer instanceof ArrayBuffer && a2.BYTES_PER_ELEMENT && !(a2.byteLength === b.byteLength && a2.every((n, i) => n === b[i]));
}
function unequalArrays(a2, b) {
  return Array.isArray(a2) && a2.length !== b.length;
}
function unequalMaps(a2, b) {
  return a2 instanceof Map && a2.size !== b.size;
}
function unequalSets(a2, b) {
  return a2 instanceof Set && (a2.size != b.size || [...a2].some((e) => !b.has(e)));
}
function unequalRegExps(a2, b) {
  return a2 instanceof RegExp && (a2.source !== b.source || a2.flags !== b.flags);
}
function isObject(a2) {
  return typeof a2 === "object" && a2 !== null;
}
function structurallyCompatibleObjects(a2, b) {
  if (typeof a2 !== "object" && typeof b !== "object" && (!a2 || !b))
    return false;
  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a2 instanceof c))
    return false;
  return a2.constructor === b.constructor;
}
function divideInt(a2, b) {
  return Math.trunc(divideFloat(a2, b));
}
function divideFloat(a2, b) {
  if (b === 0) {
    return 0;
  } else {
    return a2 / b;
  }
}
function makeError(variant, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.module = module;
  error.line = line;
  error.function = fn;
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}

// build/dev/javascript/gleam_stdlib/gleam/option.mjs
var Some = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var None = class extends CustomType {
};
function to_result(option, e) {
  if (option instanceof Some) {
    let a2 = option[0];
    return new Ok(a2);
  } else {
    return new Error(e);
  }
}

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function insert(dict2, key2, value2) {
  return map_insert(key2, value2, dict2);
}
function reverse_and_concat(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item2 = remaining.head;
      let rest = remaining.tail;
      loop$remaining = rest;
      loop$accumulator = prepend(item2, accumulator);
    }
  }
}
function do_keys_loop(loop$list, loop$acc) {
  while (true) {
    let list2 = loop$list;
    let acc = loop$acc;
    if (list2.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else {
      let first2 = list2.head;
      let rest = list2.tail;
      loop$list = rest;
      loop$acc = prepend(first2[0], acc);
    }
  }
}
function keys(dict2) {
  let list_of_pairs = map_to_list(dict2);
  return do_keys_loop(list_of_pairs, toList([]));
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
function reverse_loop(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item2 = remaining.head;
      let rest$1 = remaining.tail;
      loop$remaining = rest$1;
      loop$accumulator = prepend(item2, accumulator);
    }
  }
}
function reverse(list2) {
  return reverse_loop(list2, toList([]));
}
function map_loop(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list2 = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list2.hasLength(0)) {
      return reverse(acc);
    } else {
      let first$1 = list2.head;
      let rest$1 = list2.tail;
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = prepend(fun(first$1), acc);
    }
  }
}
function map(list2, fun) {
  return map_loop(list2, fun, toList([]));
}
function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix.hasLength(0)) {
      return suffix;
    } else {
      let first$1 = prefix.head;
      let rest$1 = prefix.tail;
      loop$prefix = rest$1;
      loop$suffix = prepend(first$1, suffix);
    }
  }
}
function concat_loop(loop$lists, loop$acc) {
  while (true) {
    let lists = loop$lists;
    let acc = loop$acc;
    if (lists.hasLength(0)) {
      return reverse(acc);
    } else {
      let list2 = lists.head;
      let further_lists = lists.tail;
      loop$lists = further_lists;
      loop$acc = reverse_and_prepend(list2, acc);
    }
  }
}
function flatten(lists) {
  return concat_loop(lists, toList([]));
}
function fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list2 = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list2.hasLength(0)) {
      return initial;
    } else {
      let x = list2.head;
      let rest$1 = list2.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, x);
      loop$fun = fun;
    }
  }
}
function index_fold_loop(loop$over, loop$acc, loop$with, loop$index) {
  while (true) {
    let over = loop$over;
    let acc = loop$acc;
    let with$ = loop$with;
    let index3 = loop$index;
    if (over.hasLength(0)) {
      return acc;
    } else {
      let first$1 = over.head;
      let rest$1 = over.tail;
      loop$over = rest$1;
      loop$acc = with$(acc, first$1, index3);
      loop$with = with$;
      loop$index = index3 + 1;
    }
  }
}
function index_fold(list2, initial, fun) {
  return index_fold_loop(list2, initial, fun, 0);
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function drop_start(loop$string, loop$num_graphemes) {
  while (true) {
    let string2 = loop$string;
    let num_graphemes = loop$num_graphemes;
    let $ = num_graphemes > 0;
    if (!$) {
      return string2;
    } else {
      let $1 = pop_grapheme(string2);
      if ($1.isOk()) {
        let string$1 = $1[0][1];
        loop$string = string$1;
        loop$num_graphemes = num_graphemes - 1;
      } else {
        return string2;
      }
    }
  }
}

// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function map2(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(fun(x));
  } else {
    let e = result[0];
    return new Error(e);
  }
}
function map_error(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else {
    let error = result[0];
    return new Error(fun(error));
  }
}
function try$(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return fun(x);
  } else {
    let e = result[0];
    return new Error(e);
  }
}
function replace_error(result, error) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else {
    return new Error(error);
  }
}

// build/dev/javascript/gleam_stdlib/gleam/dynamic.mjs
var DecodeError = class extends CustomType {
  constructor(expected, found, path2) {
    super();
    this.expected = expected;
    this.found = found;
    this.path = path2;
  }
};
function dynamic(value2) {
  return new Ok(value2);
}
function int(data) {
  return decode_int(data);
}
function any(decoders) {
  return (data) => {
    if (decoders.hasLength(0)) {
      return new Error(
        toList([new DecodeError("another type", classify_dynamic(data), toList([]))])
      );
    } else {
      let decoder = decoders.head;
      let decoders$1 = decoders.tail;
      let $ = decoder(data);
      if ($.isOk()) {
        let decoded = $[0];
        return new Ok(decoded);
      } else {
        return any(decoders$1)(data);
      }
    }
  };
}
function push_path(error, name) {
  let name$1 = identity(name);
  let decoder = any(
    toList([decode_string, (x) => {
      return map2(int(x), to_string);
    }])
  );
  let name$2 = (() => {
    let $ = decoder(name$1);
    if ($.isOk()) {
      let name$22 = $[0];
      return name$22;
    } else {
      let _pipe = toList(["<", classify_dynamic(name$1), ">"]);
      let _pipe$1 = concat(_pipe);
      return identity(_pipe$1);
    }
  })();
  return error.withFields({ path: prepend(name$2, error.path) });
}
function map_errors(result, f) {
  return map_error(
    result,
    (_capture) => {
      return map(_capture, f);
    }
  );
}
function field(name, inner_type) {
  return (value2) => {
    let missing_field_error = new DecodeError("field", "nothing", toList([]));
    return try$(
      decode_field(value2, name),
      (maybe_inner) => {
        let _pipe = maybe_inner;
        let _pipe$1 = to_result(_pipe, toList([missing_field_error]));
        let _pipe$2 = try$(_pipe$1, inner_type);
        return map_errors(
          _pipe$2,
          (_capture) => {
            return push_path(_capture, name);
          }
        );
      }
    );
  };
}

// build/dev/javascript/gleam_stdlib/dict.mjs
var referenceMap = /* @__PURE__ */ new WeakMap();
var tempDataView = new DataView(new ArrayBuffer(8));
var referenceUID = 0;
function hashByReference(o) {
  const known = referenceMap.get(o);
  if (known !== void 0) {
    return known;
  }
  const hash = referenceUID++;
  if (referenceUID === 2147483647) {
    referenceUID = 0;
  }
  referenceMap.set(o, hash);
  return hash;
}
function hashMerge(a2, b) {
  return a2 ^ b + 2654435769 + (a2 << 6) + (a2 >> 2) | 0;
}
function hashString(s) {
  let hash = 0;
  const len = s.length;
  for (let i = 0; i < len; i++) {
    hash = Math.imul(31, hash) + s.charCodeAt(i) | 0;
  }
  return hash;
}
function hashNumber(n) {
  tempDataView.setFloat64(0, n);
  const i = tempDataView.getInt32(0);
  const j = tempDataView.getInt32(4);
  return Math.imul(73244475, i >> 16 ^ i) ^ j;
}
function hashBigInt(n) {
  return hashString(n.toString());
}
function hashObject(o) {
  const proto = Object.getPrototypeOf(o);
  if (proto !== null && typeof proto.hashCode === "function") {
    try {
      const code2 = o.hashCode(o);
      if (typeof code2 === "number") {
        return code2;
      }
    } catch {
    }
  }
  if (o instanceof Promise || o instanceof WeakSet || o instanceof WeakMap) {
    return hashByReference(o);
  }
  if (o instanceof Date) {
    return hashNumber(o.getTime());
  }
  let h = 0;
  if (o instanceof ArrayBuffer) {
    o = new Uint8Array(o);
  }
  if (Array.isArray(o) || o instanceof Uint8Array) {
    for (let i = 0; i < o.length; i++) {
      h = Math.imul(31, h) + getHash(o[i]) | 0;
    }
  } else if (o instanceof Set) {
    o.forEach((v) => {
      h = h + getHash(v) | 0;
    });
  } else if (o instanceof Map) {
    o.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
  } else {
    const keys2 = Object.keys(o);
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      const v = o[k];
      h = h + hashMerge(getHash(v), hashString(k)) | 0;
    }
  }
  return h;
}
function getHash(u) {
  if (u === null)
    return 1108378658;
  if (u === void 0)
    return 1108378659;
  if (u === true)
    return 1108378657;
  if (u === false)
    return 1108378656;
  switch (typeof u) {
    case "number":
      return hashNumber(u);
    case "string":
      return hashString(u);
    case "bigint":
      return hashBigInt(u);
    case "object":
      return hashObject(u);
    case "symbol":
      return hashByReference(u);
    case "function":
      return hashByReference(u);
    default:
      return 0;
  }
}
var SHIFT = 5;
var BUCKET_SIZE = Math.pow(2, SHIFT);
var MASK = BUCKET_SIZE - 1;
var MAX_INDEX_NODE = BUCKET_SIZE / 2;
var MIN_ARRAY_NODE = BUCKET_SIZE / 4;
var ENTRY = 0;
var ARRAY_NODE = 1;
var INDEX_NODE = 2;
var COLLISION_NODE = 3;
var EMPTY = {
  type: INDEX_NODE,
  bitmap: 0,
  array: []
};
function mask(hash, shift) {
  return hash >>> shift & MASK;
}
function bitpos(hash, shift) {
  return 1 << mask(hash, shift);
}
function bitcount(x) {
  x -= x >> 1 & 1431655765;
  x = (x & 858993459) + (x >> 2 & 858993459);
  x = x + (x >> 4) & 252645135;
  x += x >> 8;
  x += x >> 16;
  return x & 127;
}
function index(bitmap, bit) {
  return bitcount(bitmap & bit - 1);
}
function cloneAndSet(arr, at, val) {
  const len = arr.length;
  const out = new Array(len);
  for (let i = 0; i < len; ++i) {
    out[i] = arr[i];
  }
  out[at] = val;
  return out;
}
function spliceIn(arr, at, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  out[g++] = val;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function spliceOut(arr, at) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  ++i;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function createNode(shift, key1, val1, key2hash, key2, val2) {
  const key1hash = getHash(key1);
  if (key1hash === key2hash) {
    return {
      type: COLLISION_NODE,
      hash: key1hash,
      array: [
        { type: ENTRY, k: key1, v: val1 },
        { type: ENTRY, k: key2, v: val2 }
      ]
    };
  }
  const addedLeaf = { val: false };
  return assoc(
    assocIndex(EMPTY, shift, key1hash, key1, val1, addedLeaf),
    shift,
    key2hash,
    key2,
    val2,
    addedLeaf
  );
}
function assoc(root, shift, hash, key2, val, addedLeaf) {
  switch (root.type) {
    case ARRAY_NODE:
      return assocArray(root, shift, hash, key2, val, addedLeaf);
    case INDEX_NODE:
      return assocIndex(root, shift, hash, key2, val, addedLeaf);
    case COLLISION_NODE:
      return assocCollision(root, shift, hash, key2, val, addedLeaf);
  }
}
function assocArray(root, shift, hash, key2, val, addedLeaf) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === void 0) {
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root.size + 1,
      array: cloneAndSet(root.array, idx, { type: ENTRY, k: key2, v: val })
    };
  }
  if (node.type === ENTRY) {
    if (isEqual(key2, node.k)) {
      if (val === node.v) {
        return root;
      }
      return {
        type: ARRAY_NODE,
        size: root.size,
        array: cloneAndSet(root.array, idx, {
          type: ENTRY,
          k: key2,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root.size,
      array: cloneAndSet(
        root.array,
        idx,
        createNode(shift + SHIFT, node.k, node.v, hash, key2, val)
      )
    };
  }
  const n = assoc(node, shift + SHIFT, hash, key2, val, addedLeaf);
  if (n === node) {
    return root;
  }
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n)
  };
}
function assocIndex(root, shift, hash, key2, val, addedLeaf) {
  const bit = bitpos(hash, shift);
  const idx = index(root.bitmap, bit);
  if ((root.bitmap & bit) !== 0) {
    const node = root.array[idx];
    if (node.type !== ENTRY) {
      const n = assoc(node, shift + SHIFT, hash, key2, val, addedLeaf);
      if (n === node) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n)
      };
    }
    const nodeKey = node.k;
    if (isEqual(key2, nodeKey)) {
      if (val === node.v) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, {
          type: ENTRY,
          k: key2,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap,
      array: cloneAndSet(
        root.array,
        idx,
        createNode(shift + SHIFT, nodeKey, node.v, hash, key2, val)
      )
    };
  } else {
    const n = root.array.length;
    if (n >= MAX_INDEX_NODE) {
      const nodes = new Array(32);
      const jdx = mask(hash, shift);
      nodes[jdx] = assocIndex(EMPTY, shift + SHIFT, hash, key2, val, addedLeaf);
      let j = 0;
      let bitmap = root.bitmap;
      for (let i = 0; i < 32; i++) {
        if ((bitmap & 1) !== 0) {
          const node = root.array[j++];
          nodes[i] = node;
        }
        bitmap = bitmap >>> 1;
      }
      return {
        type: ARRAY_NODE,
        size: n + 1,
        array: nodes
      };
    } else {
      const newArray = spliceIn(root.array, idx, {
        type: ENTRY,
        k: key2,
        v: val
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap | bit,
        array: newArray
      };
    }
  }
}
function assocCollision(root, shift, hash, key2, val, addedLeaf) {
  if (hash === root.hash) {
    const idx = collisionIndexOf(root, key2);
    if (idx !== -1) {
      const entry = root.array[idx];
      if (entry.v === val) {
        return root;
      }
      return {
        type: COLLISION_NODE,
        hash,
        array: cloneAndSet(root.array, idx, { type: ENTRY, k: key2, v: val })
      };
    }
    const size = root.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root.array, size, { type: ENTRY, k: key2, v: val })
    };
  }
  return assoc(
    {
      type: INDEX_NODE,
      bitmap: bitpos(root.hash, shift),
      array: [root]
    },
    shift,
    hash,
    key2,
    val,
    addedLeaf
  );
}
function collisionIndexOf(root, key2) {
  const size = root.array.length;
  for (let i = 0; i < size; i++) {
    if (isEqual(key2, root.array[i].k)) {
      return i;
    }
  }
  return -1;
}
function find(root, shift, hash, key2) {
  switch (root.type) {
    case ARRAY_NODE:
      return findArray(root, shift, hash, key2);
    case INDEX_NODE:
      return findIndex(root, shift, hash, key2);
    case COLLISION_NODE:
      return findCollision(root, key2);
  }
}
function findArray(root, shift, hash, key2) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === void 0) {
    return void 0;
  }
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key2);
  }
  if (isEqual(key2, node.k)) {
    return node;
  }
  return void 0;
}
function findIndex(root, shift, hash, key2) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return void 0;
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key2);
  }
  if (isEqual(key2, node.k)) {
    return node;
  }
  return void 0;
}
function findCollision(root, key2) {
  const idx = collisionIndexOf(root, key2);
  if (idx < 0) {
    return void 0;
  }
  return root.array[idx];
}
function without(root, shift, hash, key2) {
  switch (root.type) {
    case ARRAY_NODE:
      return withoutArray(root, shift, hash, key2);
    case INDEX_NODE:
      return withoutIndex(root, shift, hash, key2);
    case COLLISION_NODE:
      return withoutCollision(root, key2);
  }
}
function withoutArray(root, shift, hash, key2) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === void 0) {
    return root;
  }
  let n = void 0;
  if (node.type === ENTRY) {
    if (!isEqual(node.k, key2)) {
      return root;
    }
  } else {
    n = without(node, shift + SHIFT, hash, key2);
    if (n === node) {
      return root;
    }
  }
  if (n === void 0) {
    if (root.size <= MIN_ARRAY_NODE) {
      const arr = root.array;
      const out = new Array(root.size - 1);
      let i = 0;
      let j = 0;
      let bitmap = 0;
      while (i < idx) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      ++i;
      while (i < arr.length) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      return {
        type: INDEX_NODE,
        bitmap,
        array: out
      };
    }
    return {
      type: ARRAY_NODE,
      size: root.size - 1,
      array: cloneAndSet(root.array, idx, n)
    };
  }
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n)
  };
}
function withoutIndex(root, shift, hash, key2) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return root;
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  if (node.type !== ENTRY) {
    const n = without(node, shift + SHIFT, hash, key2);
    if (n === node) {
      return root;
    }
    if (n !== void 0) {
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n)
      };
    }
    if (root.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx)
    };
  }
  if (isEqual(key2, node.k)) {
    if (root.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx)
    };
  }
  return root;
}
function withoutCollision(root, key2) {
  const idx = collisionIndexOf(root, key2);
  if (idx < 0) {
    return root;
  }
  if (root.array.length === 1) {
    return void 0;
  }
  return {
    type: COLLISION_NODE,
    hash: root.hash,
    array: spliceOut(root.array, idx)
  };
}
function forEach(root, fn) {
  if (root === void 0) {
    return;
  }
  const items = root.array;
  const size = items.length;
  for (let i = 0; i < size; i++) {
    const item2 = items[i];
    if (item2 === void 0) {
      continue;
    }
    if (item2.type === ENTRY) {
      fn(item2.v, item2.k);
      continue;
    }
    forEach(item2, fn);
  }
}
var Dict = class _Dict {
  /**
   * @template V
   * @param {Record<string,V>} o
   * @returns {Dict<string,V>}
   */
  static fromObject(o) {
    const keys2 = Object.keys(o);
    let m = _Dict.new();
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      m = m.set(k, o[k]);
    }
    return m;
  }
  /**
   * @template K,V
   * @param {Map<K,V>} o
   * @returns {Dict<K,V>}
   */
  static fromMap(o) {
    let m = _Dict.new();
    o.forEach((v, k) => {
      m = m.set(k, v);
    });
    return m;
  }
  static new() {
    return new _Dict(void 0, 0);
  }
  /**
   * @param {undefined | Node<K,V>} root
   * @param {number} size
   */
  constructor(root, size) {
    this.root = root;
    this.size = size;
  }
  /**
   * @template NotFound
   * @param {K} key
   * @param {NotFound} notFound
   * @returns {NotFound | V}
   */
  get(key2, notFound) {
    if (this.root === void 0) {
      return notFound;
    }
    const found = find(this.root, 0, getHash(key2), key2);
    if (found === void 0) {
      return notFound;
    }
    return found.v;
  }
  /**
   * @param {K} key
   * @param {V} val
   * @returns {Dict<K,V>}
   */
  set(key2, val) {
    const addedLeaf = { val: false };
    const root = this.root === void 0 ? EMPTY : this.root;
    const newRoot = assoc(root, 0, getHash(key2), key2, val, addedLeaf);
    if (newRoot === this.root) {
      return this;
    }
    return new _Dict(newRoot, addedLeaf.val ? this.size + 1 : this.size);
  }
  /**
   * @param {K} key
   * @returns {Dict<K,V>}
   */
  delete(key2) {
    if (this.root === void 0) {
      return this;
    }
    const newRoot = without(this.root, 0, getHash(key2), key2);
    if (newRoot === this.root) {
      return this;
    }
    if (newRoot === void 0) {
      return _Dict.new();
    }
    return new _Dict(newRoot, this.size - 1);
  }
  /**
   * @param {K} key
   * @returns {boolean}
   */
  has(key2) {
    if (this.root === void 0) {
      return false;
    }
    return find(this.root, 0, getHash(key2), key2) !== void 0;
  }
  /**
   * @returns {[K,V][]}
   */
  entries() {
    if (this.root === void 0) {
      return [];
    }
    const result = [];
    this.forEach((v, k) => result.push([k, v]));
    return result;
  }
  /**
   *
   * @param {(val:V,key:K)=>void} fn
   */
  forEach(fn) {
    forEach(this.root, fn);
  }
  hashCode() {
    let h = 0;
    this.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
    return h;
  }
  /**
   * @param {unknown} o
   * @returns {boolean}
   */
  equals(o) {
    if (!(o instanceof _Dict) || this.size !== o.size) {
      return false;
    }
    try {
      this.forEach((v, k) => {
        if (!isEqual(o.get(k, !v), v)) {
          throw unequalDictSymbol;
        }
      });
      return true;
    } catch (e) {
      if (e === unequalDictSymbol) {
        return false;
      }
      throw e;
    }
  }
};
var unequalDictSymbol = Symbol();

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
var Nil = void 0;
var NOT_FOUND = {};
function identity(x) {
  return x;
}
function parse_int(value2) {
  if (/^[-+]?(\d+)$/.test(value2)) {
    return new Ok(parseInt(value2));
  } else {
    return new Error(Nil);
  }
}
function to_string(term) {
  return term.toString();
}
var segmenter = void 0;
function graphemes_iterator(string2) {
  if (globalThis.Intl && Intl.Segmenter) {
    segmenter ||= new Intl.Segmenter();
    return segmenter.segment(string2)[Symbol.iterator]();
  }
}
function pop_grapheme(string2) {
  let first2;
  const iterator = graphemes_iterator(string2);
  if (iterator) {
    first2 = iterator.next().value?.segment;
  } else {
    first2 = string2.match(/./su)?.[0];
  }
  if (first2) {
    return new Ok([first2, string2.slice(first2.length)]);
  } else {
    return new Error(Nil);
  }
}
function concat(xs) {
  let result = "";
  for (const x of xs) {
    result = result + x;
  }
  return result;
}
var unicode_whitespaces = [
  " ",
  // Space
  "	",
  // Horizontal tab
  "\n",
  // Line feed
  "\v",
  // Vertical tab
  "\f",
  // Form feed
  "\r",
  // Carriage return
  "\x85",
  // Next line
  "\u2028",
  // Line separator
  "\u2029"
  // Paragraph separator
].join("");
var trim_start_regex = new RegExp(`^[${unicode_whitespaces}]*`);
var trim_end_regex = new RegExp(`[${unicode_whitespaces}]*$`);
function new_map() {
  return Dict.new();
}
function map_to_list(map5) {
  return List.fromArray(map5.entries());
}
function map_get(map5, key2) {
  const value2 = map5.get(key2, NOT_FOUND);
  if (value2 === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value2);
}
function map_insert(key2, value2, map5) {
  return map5.set(key2, value2);
}
function classify_dynamic(data) {
  if (typeof data === "string") {
    return "String";
  } else if (typeof data === "boolean") {
    return "Bool";
  } else if (data instanceof Result) {
    return "Result";
  } else if (data instanceof List) {
    return "List";
  } else if (data instanceof BitArray) {
    return "BitArray";
  } else if (data instanceof Dict) {
    return "Dict";
  } else if (Number.isInteger(data)) {
    return "Int";
  } else if (Array.isArray(data)) {
    return `Tuple of ${data.length} elements`;
  } else if (typeof data === "number") {
    return "Float";
  } else if (data === null) {
    return "Null";
  } else if (data === void 0) {
    return "Nil";
  } else {
    const type = typeof data;
    return type.charAt(0).toUpperCase() + type.slice(1);
  }
}
function decoder_error(expected, got) {
  return decoder_error_no_classify(expected, classify_dynamic(got));
}
function decoder_error_no_classify(expected, got) {
  return new Error(
    List.fromArray([new DecodeError(expected, got, List.fromArray([]))])
  );
}
function decode_string(data) {
  return typeof data === "string" ? new Ok(data) : decoder_error("String", data);
}
function decode_int(data) {
  return Number.isInteger(data) ? new Ok(data) : decoder_error("Int", data);
}
function decode_field(value2, name) {
  const not_a_map_error = () => decoder_error("Dict", value2);
  if (value2 instanceof Dict || value2 instanceof WeakMap || value2 instanceof Map) {
    const entry = map_get(value2, name);
    return new Ok(entry.isOk() ? new Some(entry[0]) : new None());
  } else if (value2 === null) {
    return not_a_map_error();
  } else if (Object.getPrototypeOf(value2) == Object.prototype) {
    return try_get_field(value2, name, () => new Ok(new None()));
  } else {
    return try_get_field(value2, name, not_a_map_error);
  }
}
function try_get_field(value2, field2, or_else) {
  try {
    return field2 in value2 ? new Ok(new Some(value2[field2])) : or_else();
  } catch {
    return or_else();
  }
}

// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

// build/dev/javascript/lustre/lustre/effect.mjs
var Effect = class extends CustomType {
  constructor(all) {
    super();
    this.all = all;
  }
};
function none() {
  return new Effect(toList([]));
}

// build/dev/javascript/lustre/lustre/internals/vdom.mjs
var Text = class extends CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
};
var Element2 = class extends CustomType {
  constructor(key2, namespace2, tag, attrs, children2, self_closing, void$) {
    super();
    this.key = key2;
    this.namespace = namespace2;
    this.tag = tag;
    this.attrs = attrs;
    this.children = children2;
    this.self_closing = self_closing;
    this.void = void$;
  }
};
var Map2 = class extends CustomType {
  constructor(subtree) {
    super();
    this.subtree = subtree;
  }
};
var Attribute = class extends CustomType {
  constructor(x0, x1, as_property) {
    super();
    this[0] = x0;
    this[1] = x1;
    this.as_property = as_property;
  }
};
var Event2 = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
function attribute_to_event_handler(attribute2) {
  if (attribute2 instanceof Attribute) {
    return new Error(void 0);
  } else {
    let name = attribute2[0];
    let handler = attribute2[1];
    let name$1 = drop_start(name, 2);
    return new Ok([name$1, handler]);
  }
}
function do_element_list_handlers(elements2, handlers2, key2) {
  return index_fold(
    elements2,
    handlers2,
    (handlers3, element2, index3) => {
      let key$1 = key2 + "-" + to_string(index3);
      return do_handlers(element2, handlers3, key$1);
    }
  );
}
function do_handlers(loop$element, loop$handlers, loop$key) {
  while (true) {
    let element2 = loop$element;
    let handlers2 = loop$handlers;
    let key2 = loop$key;
    if (element2 instanceof Text) {
      return handlers2;
    } else if (element2 instanceof Map2) {
      let subtree = element2.subtree;
      loop$element = subtree();
      loop$handlers = handlers2;
      loop$key = key2;
    } else {
      let attrs = element2.attrs;
      let children2 = element2.children;
      let handlers$1 = fold(
        attrs,
        handlers2,
        (handlers3, attr) => {
          let $ = attribute_to_event_handler(attr);
          if ($.isOk()) {
            let name = $[0][0];
            let handler = $[0][1];
            return insert(handlers3, key2 + "-" + name, handler);
          } else {
            return handlers3;
          }
        }
      );
      return do_element_list_handlers(children2, handlers$1, key2);
    }
  }
}
function handlers(element2) {
  return do_handlers(element2, new_map(), "0");
}

// build/dev/javascript/lustre/lustre/attribute.mjs
function attribute(name, value2) {
  return new Attribute(name, identity(value2), false);
}
function property(name, value2) {
  return new Attribute(name, identity(value2), true);
}
function on(name, handler) {
  return new Event2("on" + name, handler);
}
function class$(name) {
  return attribute("class", name);
}
function disabled(is_disabled) {
  return property("disabled", is_disabled);
}
function href(uri) {
  return attribute("href", uri);
}

// build/dev/javascript/lustre/lustre/element.mjs
function element(tag, attrs, children2) {
  if (tag === "area") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "base") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "br") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "col") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "embed") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "hr") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "img") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "input") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "link") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "meta") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "param") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "source") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "track") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "wbr") {
    return new Element2("", "", tag, attrs, toList([]), false, true);
  } else {
    return new Element2("", "", tag, attrs, children2, false, false);
  }
}
function namespaced(namespace2, tag, attrs, children2) {
  return new Element2("", namespace2, tag, attrs, children2, false, false);
}
function text(content) {
  return new Text(content);
}

// build/dev/javascript/gleam_stdlib/gleam/set.mjs
var Set2 = class extends CustomType {
  constructor(dict2) {
    super();
    this.dict = dict2;
  }
};
function new$2() {
  return new Set2(new_map());
}

// build/dev/javascript/lustre/lustre/internals/patch.mjs
var Diff = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Emit = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Init = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
function is_empty_element_diff(diff2) {
  return isEqual(diff2.created, new_map()) && isEqual(
    diff2.removed,
    new$2()
  ) && isEqual(diff2.updated, new_map());
}

// build/dev/javascript/lustre/lustre/internals/runtime.mjs
var Attrs = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Batch = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Debug = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Dispatch = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Emit2 = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Event3 = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Shutdown = class extends CustomType {
};
var Subscribe = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Unsubscribe = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var ForceModel = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};

// build/dev/javascript/lustre/vdom.ffi.mjs
if (globalThis.customElements && !globalThis.customElements.get("lustre-fragment")) {
  globalThis.customElements.define(
    "lustre-fragment",
    class LustreFragment extends HTMLElement {
      constructor() {
        super();
      }
    }
  );
}
function morph(prev, next, dispatch) {
  let out;
  let stack = [{ prev, next, parent: prev.parentNode }];
  while (stack.length) {
    let { prev: prev2, next: next2, parent } = stack.pop();
    while (next2.subtree !== void 0)
      next2 = next2.subtree();
    if (next2.content !== void 0) {
      if (!prev2) {
        const created = document.createTextNode(next2.content);
        parent.appendChild(created);
        out ??= created;
      } else if (prev2.nodeType === Node.TEXT_NODE) {
        if (prev2.textContent !== next2.content)
          prev2.textContent = next2.content;
        out ??= prev2;
      } else {
        const created = document.createTextNode(next2.content);
        parent.replaceChild(created, prev2);
        out ??= created;
      }
    } else if (next2.tag !== void 0) {
      const created = createElementNode({
        prev: prev2,
        next: next2,
        dispatch,
        stack
      });
      if (!prev2) {
        parent.appendChild(created);
      } else if (prev2 !== created) {
        parent.replaceChild(created, prev2);
      }
      out ??= created;
    }
  }
  return out;
}
function createElementNode({ prev, next, dispatch, stack }) {
  const namespace2 = next.namespace || "http://www.w3.org/1999/xhtml";
  const canMorph = prev && prev.nodeType === Node.ELEMENT_NODE && prev.localName === next.tag && prev.namespaceURI === (next.namespace || "http://www.w3.org/1999/xhtml");
  const el = canMorph ? prev : namespace2 ? document.createElementNS(namespace2, next.tag) : document.createElement(next.tag);
  let handlersForEl;
  if (!registeredHandlers.has(el)) {
    const emptyHandlers = /* @__PURE__ */ new Map();
    registeredHandlers.set(el, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el);
  }
  const prevHandlers = canMorph ? new Set(handlersForEl.keys()) : null;
  const prevAttributes = canMorph ? new Set(Array.from(prev.attributes, (a2) => a2.name)) : null;
  let className = null;
  let style3 = null;
  let innerHTML = null;
  if (canMorph && next.tag === "textarea") {
    const innertText = next.children[Symbol.iterator]().next().value?.content;
    if (innertText !== void 0)
      el.value = innertText;
  }
  const delegated = [];
  for (const attr of next.attrs) {
    const name = attr[0];
    const value2 = attr[1];
    if (attr.as_property) {
      if (el[name] !== value2)
        el[name] = value2;
      if (canMorph)
        prevAttributes.delete(name);
    } else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch(value2, eventName === "input");
      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name.startsWith("data-lustre-on-")) {
      const eventName = name.slice(15);
      const callback = dispatch(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el.setAttribute(name, value2);
      if (canMorph) {
        prevHandlers.delete(eventName);
        prevAttributes.delete(name);
      }
    } else if (name.startsWith("delegate:data-") || name.startsWith("delegate:aria-")) {
      el.setAttribute(name, value2);
      delegated.push([name.slice(10), value2]);
    } else if (name === "class") {
      className = className === null ? value2 : className + " " + value2;
    } else if (name === "style") {
      style3 = style3 === null ? value2 : style3 + value2;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value2;
    } else {
      if (el.getAttribute(name) !== value2)
        el.setAttribute(name, value2);
      if (name === "value" || name === "selected")
        el[name] = value2;
      if (canMorph)
        prevAttributes.delete(name);
    }
  }
  if (className !== null) {
    el.setAttribute("class", className);
    if (canMorph)
      prevAttributes.delete("class");
  }
  if (style3 !== null) {
    el.setAttribute("style", style3);
    if (canMorph)
      prevAttributes.delete("style");
  }
  if (canMorph) {
    for (const attr of prevAttributes) {
      el.removeAttribute(attr);
    }
    for (const eventName of prevHandlers) {
      handlersForEl.delete(eventName);
      el.removeEventListener(eventName, lustreGenericEventHandler);
    }
  }
  if (next.tag === "slot") {
    window.queueMicrotask(() => {
      for (const child of el.assignedElements()) {
        for (const [name, value2] of delegated) {
          if (!child.hasAttribute(name)) {
            child.setAttribute(name, value2);
          }
        }
      }
    });
  }
  if (next.key !== void 0 && next.key !== "") {
    el.setAttribute("data-lustre-key", next.key);
  } else if (innerHTML !== null) {
    el.innerHTML = innerHTML;
    return el;
  }
  let prevChild = el.firstChild;
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = children(next).next().value;
  if (canMorph && firstChild !== void 0 && // Explicit checks are more verbose but truthy checks force a bunch of comparisons
  // we don't care about: it's never gonna be a number etc.
  firstChild.key !== void 0 && firstChild.key !== "") {
    seenKeys = /* @__PURE__ */ new Set();
    keyedChildren = getKeyedChildren(prev);
    incomingKeyedChildren = getKeyedChildren(next);
    for (const child of children(next)) {
      prevChild = diffKeyedChild(
        prevChild,
        child,
        el,
        stack,
        incomingKeyedChildren,
        keyedChildren,
        seenKeys
      );
    }
  } else {
    for (const child of children(next)) {
      stack.unshift({ prev: prevChild, next: child, parent: el });
      prevChild = prevChild?.nextSibling;
    }
  }
  while (prevChild) {
    const next2 = prevChild.nextSibling;
    el.removeChild(prevChild);
    prevChild = next2;
  }
  return el;
}
var registeredHandlers = /* @__PURE__ */ new WeakMap();
function lustreGenericEventHandler(event2) {
  const target2 = event2.currentTarget;
  if (!registeredHandlers.has(target2)) {
    target2.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  const handlersForEventTarget = registeredHandlers.get(target2);
  if (!handlersForEventTarget.has(event2.type)) {
    target2.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  handlersForEventTarget.get(event2.type)(event2);
}
function lustreServerEventHandler(event2) {
  const el = event2.currentTarget;
  const tag = el.getAttribute(`data-lustre-on-${event2.type}`);
  const data = JSON.parse(el.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el.getAttribute("data-lustre-include") || "[]");
  switch (event2.type) {
    case "input":
    case "change":
      include.push("target.value");
      break;
  }
  return {
    tag,
    data: include.reduce(
      (data2, property2) => {
        const path2 = property2.split(".");
        for (let i = 0, o = data2, e = event2; i < path2.length; i++) {
          if (i === path2.length - 1) {
            o[path2[i]] = e[path2[i]];
          } else {
            o[path2[i]] ??= {};
            e = e[path2[i]];
            o = o[path2[i]];
          }
        }
        return data2;
      },
      { data }
    )
  };
}
function getKeyedChildren(el) {
  const keyedChildren = /* @__PURE__ */ new Map();
  if (el) {
    for (const child of children(el)) {
      const key2 = child?.key || child?.getAttribute?.("data-lustre-key");
      if (key2)
        keyedChildren.set(key2, child);
    }
  }
  return keyedChildren;
}
function diffKeyedChild(prevChild, child, el, stack, incomingKeyedChildren, keyedChildren, seenKeys) {
  while (prevChild && !incomingKeyedChildren.has(prevChild.getAttribute("data-lustre-key"))) {
    const nextChild = prevChild.nextSibling;
    el.removeChild(prevChild);
    prevChild = nextChild;
  }
  if (keyedChildren.size === 0) {
    stack.unshift({ prev: prevChild, next: child, parent: el });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }
  if (seenKeys.has(child.key)) {
    console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
    stack.unshift({ prev: null, next: child, parent: el });
    return prevChild;
  }
  seenKeys.add(child.key);
  const keyedChild = keyedChildren.get(child.key);
  if (!keyedChild && !prevChild) {
    stack.unshift({ prev: null, next: child, parent: el });
    return prevChild;
  }
  if (!keyedChild && prevChild !== null) {
    const placeholder = document.createTextNode("");
    el.insertBefore(placeholder, prevChild);
    stack.unshift({ prev: placeholder, next: child, parent: el });
    return prevChild;
  }
  if (!keyedChild || keyedChild === prevChild) {
    stack.unshift({ prev: prevChild, next: child, parent: el });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }
  el.insertBefore(keyedChild, prevChild);
  stack.unshift({ prev: keyedChild, next: child, parent: el });
  return prevChild;
}
function* children(element2) {
  for (const child of element2.children) {
    yield* forceChild(child);
  }
}
function* forceChild(element2) {
  if (element2.subtree !== void 0) {
    yield* forceChild(element2.subtree());
  } else {
    yield element2;
  }
}

// build/dev/javascript/lustre/lustre.ffi.mjs
var LustreClientApplication = class _LustreClientApplication {
  /**
   * @template Flags
   *
   * @param {object} app
   * @param {(flags: Flags) => [Model, Lustre.Effect<Msg>]} app.init
   * @param {(msg: Msg, model: Model) => [Model, Lustre.Effect<Msg>]} app.update
   * @param {(model: Model) => Lustre.Element<Msg>} app.view
   * @param {string | HTMLElement} selector
   * @param {Flags} flags
   *
   * @returns {Gleam.Ok<(action: Lustre.Action<Lustre.Client, Msg>>) => void>}
   */
  static start({ init: init3, update: update2, view: view2 }, selector, flags) {
    if (!is_browser())
      return new Error(new NotABrowser());
    const root = selector instanceof HTMLElement ? selector : document.querySelector(selector);
    if (!root)
      return new Error(new ElementNotFound(selector));
    const app = new _LustreClientApplication(root, init3(flags), update2, view2);
    return new Ok((action) => app.send(action));
  }
  /**
   * @param {Element} root
   * @param {[Model, Lustre.Effect<Msg>]} init
   * @param {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} update
   * @param {(model: Model) => Lustre.Element<Msg>} view
   *
   * @returns {LustreClientApplication}
   */
  constructor(root, [init3, effects], update2, view2) {
    this.root = root;
    this.#model = init3;
    this.#update = update2;
    this.#view = view2;
    this.#tickScheduled = window.requestAnimationFrame(
      () => this.#tick(effects.all.toArray(), true)
    );
  }
  /** @type {Element} */
  root;
  /**
   * @param {Lustre.Action<Lustre.Client, Msg>} action
   *
   * @returns {void}
   */
  send(action) {
    if (action instanceof Debug) {
      if (action[0] instanceof ForceModel) {
        this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
        this.#queue = [];
        this.#model = action[0][0];
        const vdom = this.#view(this.#model);
        const dispatch = (handler, immediate = false) => (event2) => {
          const result = handler(event2);
          if (result instanceof Ok) {
            this.send(new Dispatch(result[0], immediate));
          }
        };
        const prev = this.root.firstChild ?? this.root.appendChild(document.createTextNode(""));
        morph(prev, vdom, dispatch);
      }
    } else if (action instanceof Dispatch) {
      const msg = action[0];
      const immediate = action[1] ?? false;
      this.#queue.push(msg);
      if (immediate) {
        this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
        this.#tick();
      } else if (!this.#tickScheduled) {
        this.#tickScheduled = window.requestAnimationFrame(() => this.#tick());
      }
    } else if (action instanceof Emit2) {
      const event2 = action[0];
      const data = action[1];
      this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
    } else if (action instanceof Shutdown) {
      this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
      this.#model = null;
      this.#update = null;
      this.#view = null;
      this.#queue = null;
      while (this.root.firstChild) {
        this.root.firstChild.remove();
      }
    }
  }
  /** @type {Model} */
  #model;
  /** @type {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} */
  #update;
  /** @type {(model: Model) => Lustre.Element<Msg>} */
  #view;
  /** @type {Array<Msg>} */
  #queue = [];
  /** @type {number | undefined} */
  #tickScheduled;
  /**
   * @param {Lustre.Effect<Msg>[]} effects
   */
  #tick(effects = []) {
    this.#tickScheduled = void 0;
    this.#flush(effects);
    const vdom = this.#view(this.#model);
    const dispatch = (handler, immediate = false) => (event2) => {
      const result = handler(event2);
      if (result instanceof Ok) {
        this.send(new Dispatch(result[0], immediate));
      }
    };
    const prev = this.root.firstChild ?? this.root.appendChild(document.createTextNode(""));
    morph(prev, vdom, dispatch);
  }
  #flush(effects = []) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect] = this.#update(this.#model, msg);
      effects = effects.concat(effect.all.toArray());
      this.#model = next;
    }
    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit2 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select = () => {
      };
      const root = this.root;
      effect({ dispatch, emit: emit2, select, root });
    }
    if (this.#queue.length > 0) {
      this.#flush(effects);
    }
  }
};
var start = LustreClientApplication.start;
var LustreServerApplication = class _LustreServerApplication {
  static start({ init: init3, update: update2, view: view2, on_attribute_change }, flags) {
    const app = new _LustreServerApplication(
      init3(flags),
      update2,
      view2,
      on_attribute_change
    );
    return new Ok((action) => app.send(action));
  }
  constructor([model, effects], update2, view2, on_attribute_change) {
    this.#model = model;
    this.#update = update2;
    this.#view = view2;
    this.#html = view2(model);
    this.#onAttributeChange = on_attribute_change;
    this.#renderers = /* @__PURE__ */ new Map();
    this.#handlers = handlers(this.#html);
    this.#tick(effects.all.toArray());
  }
  send(action) {
    if (action instanceof Attrs) {
      for (const attr of action[0]) {
        const decoder = this.#onAttributeChange.get(attr[0]);
        if (!decoder)
          continue;
        const msg = decoder(attr[1]);
        if (msg instanceof Error)
          continue;
        this.#queue.push(msg);
      }
      this.#tick();
    } else if (action instanceof Batch) {
      this.#queue = this.#queue.concat(action[0].toArray());
      this.#tick(action[1].all.toArray());
    } else if (action instanceof Debug) {
    } else if (action instanceof Dispatch) {
      this.#queue.push(action[0]);
      this.#tick();
    } else if (action instanceof Emit2) {
      const event2 = new Emit(action[0], action[1]);
      for (const [_, renderer] of this.#renderers) {
        renderer(event2);
      }
    } else if (action instanceof Event3) {
      const handler = this.#handlers.get(action[0]);
      if (!handler)
        return;
      const msg = handler(action[1]);
      if (msg instanceof Error)
        return;
      this.#queue.push(msg[0]);
      this.#tick();
    } else if (action instanceof Subscribe) {
      const attrs = keys(this.#onAttributeChange);
      const patch = new Init(attrs, this.#html);
      this.#renderers = this.#renderers.set(action[0], action[1]);
      action[1](patch);
    } else if (action instanceof Unsubscribe) {
      this.#renderers = this.#renderers.delete(action[0]);
    }
  }
  #model;
  #update;
  #queue;
  #view;
  #html;
  #renderers;
  #handlers;
  #onAttributeChange;
  #tick(effects = []) {
    this.#flush(effects);
    const vdom = this.#view(this.#model);
    const diff2 = elements(this.#html, vdom);
    if (!is_empty_element_diff(diff2)) {
      const patch = new Diff(diff2);
      for (const [_, renderer] of this.#renderers) {
        renderer(patch);
      }
    }
    this.#html = vdom;
    this.#handlers = diff2.handlers;
  }
  #flush(effects = []) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect] = this.#update(this.#model, msg);
      effects = effects.concat(effect.all.toArray());
      this.#model = next;
    }
    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit2 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select = () => {
      };
      const root = null;
      effect({ dispatch, emit: emit2, select, root });
    }
    if (this.#queue.length > 0) {
      this.#flush(effects);
    }
  }
};
var start_server_application = LustreServerApplication.start;
var is_browser = () => globalThis.window && window.document;

// build/dev/javascript/lustre/lustre.mjs
var App = class extends CustomType {
  constructor(init3, update2, view2, on_attribute_change) {
    super();
    this.init = init3;
    this.update = update2;
    this.view = view2;
    this.on_attribute_change = on_attribute_change;
  }
};
var ElementNotFound = class extends CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
};
var NotABrowser = class extends CustomType {
};
function application(init3, update2, view2) {
  return new App(init3, update2, view2, new None());
}
function simple(init3, update2, view2) {
  let init$1 = (flags) => {
    return [init3(flags), none()];
  };
  let update$1 = (model, msg) => {
    return [update2(model, msg), none()];
  };
  return application(init$1, update$1, view2);
}
function start2(app, selector, flags) {
  return guard(
    !is_browser(),
    new Error(new NotABrowser()),
    () => {
      return start(app, selector, flags);
    }
  );
}

// build/dev/javascript/lustre/lustre/element/html.mjs
function text2(content) {
  return text(content);
}
function style2(attrs, css) {
  return element("style", attrs, toList([text2(css)]));
}
function h1(attrs, children2) {
  return element("h1", attrs, children2);
}
function div(attrs, children2) {
  return element("div", attrs, children2);
}
function p(attrs, children2) {
  return element("p", attrs, children2);
}
function a(attrs, children2) {
  return element("a", attrs, children2);
}
function span(attrs, children2) {
  return element("span", attrs, children2);
}
function button(attrs, children2) {
  return element("button", attrs, children2);
}

// build/dev/javascript/lustre/lustre/event.mjs
function on2(name, handler) {
  return on(name, handler);
}

// build/dev/javascript/plinth/document_ffi.mjs
function querySelector(query) {
  let found = document.querySelector(query);
  if (!found) {
    return new Error();
  }
  return new Ok(found);
}
function addEventListener(type, listener) {
  return document.addEventListener(type, listener);
}

// build/dev/javascript/plinth/element_ffi.mjs
function cast(raw) {
  if (raw instanceof Element) {
    return new Ok(raw);
  } else {
    return new Error();
  }
}
function getAttribute(element2, name) {
  let attribute2 = element2.getAttribute(name);
  if (attribute2) {
    return new Ok(attribute2);
  }
  return new Error();
}
function setAttribute(element2, name, value2) {
  element2.setAttribute(name, value2);
}

// build/dev/javascript/plinth/plinth/browser/element.mjs
function cast2(raw) {
  let $ = cast(raw);
  if ($.isOk()) {
    let element2 = $[0];
    return new Ok(element2);
  } else {
    return new Error(
      new DecodeError("Element", classify_dynamic(raw), toList([]))
    );
  }
}

// build/dev/javascript/popcicle/popcicle_ffi.mjs
function initPopcicle(config) {
  if (globalThis.popcicle)
    return;
  globalThis.popcicle = {
    popups: [],
    config
  };
}
function getPopcicleConfig() {
  try {
    if (globalThis.popcicle.config) {
      return new Ok(globalThis.popcicle.config);
    } else {
      return new Error(void 0);
    }
  } catch {
    return new Error(void 0);
  }
}
function getPopcicles() {
  return toList(globalThis.popcicle.popups);
}
function registerPopcicle(a2) {
  if (!globalThis.popcicle)
    throw new Error("Popcicle must be initialized before calling register");
  const id = globalThis.popcicle.popups.length;
  globalThis.popcicle.popups.push([id, a2]);
  return id;
}
function getFirstParentWithAttrib(elem, attrib) {
  let currentElement = elem;
  let depth = 0;
  while (currentElement && depth < 20) {
    if (currentElement.hasAttribute(attrib)) {
      return new Ok(currentElement);
    }
    currentElement = currentElement.parentElement;
    depth++;
  }
  return new Error(void 0);
}
function getElementBoundingBox(elem, index3) {
  try {
    const value2 = elem.getBoundingClientRect()[index3];
    if (value2) {
      return new Ok(value2);
    } else {
      return new Error(void 0);
    }
  } catch {
    return new Error(void 0);
  }
}
function getElementLeft(elem) {
  return getElementBoundingBox(elem, "left");
}
function getElementRight(elem) {
  return getElementBoundingBox(elem, "right");
}
function getElementTop(elem) {
  return getElementBoundingBox(elem, "top");
}
function getElementBottom(elem) {
  return getElementBoundingBox(elem, "bottom");
}
function getElementWidth(elem) {
  return getElementBoundingBox(elem, "width");
}
function getElementHeight(elem) {
  return getElementBoundingBox(elem, "height");
}
function isMouseOver(event2, element2) {
  const rect = element2.getBoundingClientRect();
  return event2.clientX >= rect.left && event2.clientX <= rect.right && event2.clientY >= rect.top && event2.clientY <= rect.bottom;
}
function contains(element2, other) {
  return element2.contains(other);
}

// build/dev/javascript/popcicle/popcicle.mjs
var TopLeft = class extends CustomType {
};
var TopCenter = class extends CustomType {
};
var TopRight = class extends CustomType {
};
var RightTop = class extends CustomType {
};
var RightCenter = class extends CustomType {
};
var RightBottom = class extends CustomType {
};
var BottomLeft = class extends CustomType {
};
var BottomCenter = class extends CustomType {
};
var BottomRight = class extends CustomType {
};
var LeftCenter = class extends CustomType {
};
var LeftBottom = class extends CustomType {
};
var PopcicleConfig = class extends CustomType {
  constructor(css, click_gap, hover_gap) {
    super();
    this.css = css;
    this.click_gap = click_gap;
    this.hover_gap = hover_gap;
  }
};
var Click = class extends CustomType {
};
var Hover = class extends CustomType {
};
function default_config() {
  return new PopcicleConfig(
    '\ndiv[data-popcicle-open] {\n  position: absolute;\n  z-index: 9999;\n  transition: all;\n  transition-duration: 200ms;\n}\n\ndiv[data-popcicle-open="0"] {\n  opacity: 0;\n}\n\ndiv[data-popcicle-open="1"] {\n  opacity: 1;\n}',
    4,
    -2
  );
}
function initialize(config, cb) {
  let $ = initPopcicle(config);
  addEventListener(
    "mousedown",
    (event2) => {
      let $1 = try$(
        field("target", dynamic)(identity(event2)),
        (target2) => {
          map(
            getPopcicles(),
            (popcicle2) => {
              return try$(
                querySelector(
                  'div[data-popcicle-id="' + to_string(popcicle2[0]) + '"]'
                ),
                (popcicle3) => {
                  return try$(
                    (() => {
                      let _pipe = cast2(target2);
                      return replace_error(_pipe, void 0);
                    })(),
                    (target3) => {
                      return guard(
                        contains(popcicle3, target3),
                        new Error(void 0),
                        () => {
                          return new Ok(
                            setAttribute(popcicle3, "data-popcicle-open", "0")
                          );
                        }
                      );
                    }
                  );
                }
              );
            }
          );
          return new Error(toList([]));
        }
      );
      return void 0;
    }
  );
  addEventListener(
    "mousemove",
    (event2) => {
      let $1 = (() => {
        map(
          getPopcicles(),
          (popcicle2) => {
            return try$(
              querySelector(
                'div[data-popcicle-target-id="' + to_string(popcicle2[0]) + '"]'
              ),
              (target2) => {
                return try$(
                  querySelector(
                    'div[data-popcicle-id="' + to_string(popcicle2[0]) + '"], data[data-popcicle-show-on="hover"]'
                  ),
                  (popcicle3) => {
                    return try$(
                      getAttribute(
                        popcicle3,
                        "data-popcicle-show-on"
                      ),
                      (show_on) => {
                        return guard(
                          show_on !== "hover",
                          new Error(void 0),
                          () => {
                            return guard(
                              isMouseOver(event2, popcicle3) || isMouseOver(
                                event2,
                                target2
                              ),
                              new Error(void 0),
                              () => {
                                setAttribute(
                                  popcicle3,
                                  "data-popcicle-open",
                                  "0"
                                );
                                return new Error(void 0);
                              }
                            );
                          }
                        );
                      }
                    );
                  }
                );
              }
            );
          }
        );
        return new Error(toList([]));
      })();
      return void 0;
    }
  );
  return div(
    toList([]),
    flatten(
      toList([
        toList([style2(toList([]), config.css), cb()]),
        (() => {
          let _pipe = getPopcicles();
          return map(
            _pipe,
            (popcicle2) => {
              return div(
                toList([
                  attribute("data-popcicle-id", to_string(popcicle2[0])),
                  attribute("data-popcicle-open", "0")
                ]),
                toList([popcicle2[1]])
              );
            }
          );
        })()
      ])
    )
  );
}
function tuples_to_style(loop$in, loop$acc) {
  while (true) {
    let in$ = loop$in;
    let acc = loop$acc;
    if (in$.atLeastLength(1)) {
      let item2 = in$.head;
      let rest = in$.tail;
      loop$in = rest;
      loop$acc = acc + item2[0] + ": " + item2[1] + "; ";
    } else {
      return acc;
    }
  }
}
function get_position_values(position, relative, popcicle2, show_on) {
  return try$(
    (() => {
      let _pipe = getElementRight(relative);
      return replace_error(_pipe, toList([]));
    })(),
    (right) => {
      return try$(
        (() => {
          let _pipe = getElementTop(relative);
          return replace_error(_pipe, toList([]));
        })(),
        (top) => {
          return try$(
            (() => {
              let _pipe = getElementLeft(relative);
              return replace_error(_pipe, toList([]));
            })(),
            (left) => {
              return try$(
                (() => {
                  let _pipe = getElementBottom(relative);
                  return replace_error(_pipe, toList([]));
                })(),
                (bottom) => {
                  return try$(
                    (() => {
                      let _pipe = getElementWidth(popcicle2);
                      return replace_error(_pipe, toList([]));
                    })(),
                    (popcicle_width) => {
                      return try$(
                        (() => {
                          let _pipe = getElementHeight(popcicle2);
                          return replace_error(_pipe, toList([]));
                        })(),
                        (popcicle_height) => {
                          return try$(
                            (() => {
                              let _pipe = getElementWidth(relative);
                              return replace_error(_pipe, toList([]));
                            })(),
                            (target_width) => {
                              return try$(
                                (() => {
                                  let _pipe = getElementHeight(relative);
                                  return replace_error(
                                    _pipe,
                                    toList([])
                                  );
                                })(),
                                (target_height) => {
                                  return try$(
                                    (() => {
                                      let _pipe = getPopcicleConfig();
                                      return replace_error(
                                        _pipe,
                                        toList([])
                                      );
                                    })(),
                                    (config) => {
                                      let gap = (() => {
                                        if (show_on instanceof Click) {
                                          return config.click_gap;
                                        } else {
                                          return config.hover_gap;
                                        }
                                      })();
                                      let res = (() => {
                                        if (position instanceof TopLeft) {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = top - popcicle_height - gap;
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = left - popcicle_width + target_width;
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        } else if (position instanceof TopCenter) {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = top - popcicle_height - gap;
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = left - divideInt(
                                                  popcicle_width - target_width,
                                                  2
                                                );
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        } else if (position instanceof TopRight) {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = top - popcicle_height - gap;
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = left;
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        } else if (position instanceof RightBottom) {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = top;
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = right + gap;
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        } else if (position instanceof RightCenter) {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = top - divideInt(
                                                  popcicle_height - target_height,
                                                  2
                                                );
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = right + gap;
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        } else if (position instanceof RightTop) {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = top - popcicle_height + target_height;
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = right + gap;
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        } else if (position instanceof BottomLeft) {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = bottom + gap;
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = left - popcicle_width + target_width;
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        } else if (position instanceof BottomCenter) {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = bottom + gap;
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = left - divideInt(
                                                  popcicle_width - target_width,
                                                  2
                                                );
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        } else if (position instanceof BottomRight) {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = bottom + gap;
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = left;
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        } else if (position instanceof LeftBottom) {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = top;
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = left - popcicle_width - gap;
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        } else if (position instanceof LeftCenter) {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = top - divideInt(
                                                  popcicle_height - target_height,
                                                  2
                                                );
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = left - popcicle_width - gap;
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        } else {
                                          return toList([
                                            [
                                              "top",
                                              (() => {
                                                let _pipe = top - popcicle_height + target_height;
                                                return to_string(_pipe);
                                              })()
                                            ],
                                            [
                                              "left",
                                              (() => {
                                                let _pipe = left - popcicle_width - gap;
                                                return to_string(_pipe);
                                              })()
                                            ]
                                          ]);
                                        }
                                      })();
                                      return new Ok(
                                        (() => {
                                          let _pipe = res;
                                          let _pipe$1 = map(
                                            _pipe,
                                            (a2) => {
                                              return [
                                                a2[0],
                                                (() => {
                                                  let $ = a2[0];
                                                  if ($ === "left") {
                                                    return a2[1] + "px";
                                                  } else if ($ === "top") {
                                                    return a2[1] + "px";
                                                  } else {
                                                    return a2[1];
                                                  }
                                                })()
                                              ];
                                            }
                                          );
                                          return tuples_to_style(_pipe$1, "");
                                        })()
                                      );
                                    }
                                  );
                                }
                              );
                            }
                          );
                        }
                      );
                    }
                  );
                }
              );
            }
          );
        }
      );
    }
  );
}
function popcicle(child, position, popcicle2, show_on) {
  let open_popcicle = (event2) => {
    return try$(
      field("target", dynamic)(event2),
      (button2) => {
        return try$(
          (() => {
            let _pipe = getFirstParentWithAttrib(
              button2,
              "data-popcicle-target-id"
            );
            return replace_error(_pipe, toList([]));
          })(),
          (button3) => {
            return try$(
              (() => {
                let _pipe = cast2(button3);
                return replace_error(_pipe, toList([]));
              })(),
              (button4) => {
                return try$(
                  (() => {
                    let _pipe = getAttribute(button4, "data-popcicle-target-id");
                    return replace_error(_pipe, toList([]));
                  })(),
                  (popcicle_target_id) => {
                    return try$(
                      (() => {
                        let _pipe = querySelector(
                          "div[data-popcicle-id='" + popcicle_target_id + "']"
                        );
                        return replace_error(_pipe, toList([]));
                      })(),
                      (popcicle3) => {
                        return try$(
                          get_position_values(
                            position,
                            button4,
                            popcicle3,
                            show_on
                          ),
                          (styles) => {
                            setAttribute(popcicle3, "data-popcicle-open", "1");
                            let $ = parse_int(popcicle_target_id);
                            if (!$.isOk()) {
                              throw makeError(
                                "let_assert",
                                "popcicle",
                                212,
                                "",
                                "Pattern match failed, no pattern matched the value.",
                                { value: $ }
                              );
                            }
                            let id2 = $[0];
                            setAttribute(
                              popcicle3,
                              "style",
                              styles + "; z-index: " + to_string(
                                9999 - id2
                              )
                            );
                            setAttribute(
                              popcicle3,
                              "data-popcicle-show-on",
                              (() => {
                                if (show_on instanceof Click) {
                                  return "click";
                                } else {
                                  return "hover";
                                }
                              })()
                            );
                            return new Error(toList([]));
                          }
                        );
                      }
                    );
                  }
                );
              }
            );
          }
        );
      }
    );
  };
  let id = registerPopcicle(popcicle2);
  return div(
    toList([
      attribute("data-popcicle-target-id", to_string(id)),
      (() => {
        if (show_on instanceof Click) {
          return on2("click", open_popcicle);
        } else {
          return on2("mouseover", open_popcicle);
        }
      })()
    ]),
    toList([child])
  );
}

// build/dev/javascript/lustre/lustre/element/svg.mjs
var namespace = "http://www.w3.org/2000/svg";
function svg(attrs, children2) {
  return namespaced(namespace, "svg", attrs, children2);
}
function path(attrs) {
  return namespaced(namespace, "path", attrs, toList([]));
}

// build/dev/javascript/www/lucide_lustre.mjs
function chevron_right(attributes) {
  return svg(
    prepend(
      attribute("stroke-linejoin", "round"),
      prepend(
        attribute("stroke-linecap", "round"),
        prepend(
          attribute("stroke-width", "2"),
          prepend(
            attribute("stroke", "currentColor"),
            prepend(
              attribute("fill", "none"),
              prepend(
                attribute("viewBox", "0 0 24 24"),
                prepend(
                  attribute("height", "24"),
                  prepend(attribute("width", "24"), attributes)
                )
              )
            )
          )
        )
      )
    ),
    toList([path(toList([attribute("d", "m9 18 6-6-6-6")]))])
  );
}

// build/dev/javascript/www/popcicle/components/dropdown_menu.mjs
function dropdown_menu(child, position, attributes, children2, show_on) {
  return popcicle(
    child,
    position,
    div(
      flatten(
        toList([
          toList([
            class$(
              "p-1 min-w-[8rem] outline-none shadow-md bg-white rounded-md border border-zinc-200 overflow-hidden w-56 transition-all duration-200"
            )
          ]),
          attributes
        ])
      ),
      children2
    ),
    show_on
  );
}
function item(attributes, children2) {
  return div(
    flatten(
      toList([
        toList([
          class$(
            "relative flex cursor-pointer hover:bg-zinc-100 text-zinc-900 select-none items-center gap-2 rounded-sm px-2 py-1.5 text-sm outline-none transition-colors focus:bg-zinc-50 focus:text-zinc-800 data-[disabled]:pointer-events-none data-[disabled]:opacity-50 [&>svg]:size-4 [&>svg]:shrink-0"
          )
        ]),
        attributes
      ])
    ),
    children2
  );
}
function label(attributes, children2) {
  return div(
    flatten(
      toList([toList([class$("px-2 py-1.5 text-sm font-semibold")]), attributes])
    ),
    children2
  );
}
function separator(attributes) {
  return div(
    flatten(
      toList([toList([class$("-mx-1 my-1 h-px bg-zinc-100")]), attributes])
    ),
    toList([])
  );
}
function shortcut(attributes, children2) {
  return span(
    flatten(
      toList([
        toList([class$("ml-auto text-xs tracking-widest opacity-60")]),
        attributes
      ])
    ),
    children2
  );
}

// build/dev/javascript/www/www.mjs
var Incr = class extends CustomType {
};
function init2(initial_count) {
  let $ = initial_count < 0;
  if ($) {
    return 0;
  } else {
    return initial_count;
  }
}
function update(model, msg) {
  if (msg instanceof Incr) {
    return model + 1;
  } else {
    return model - 1;
  }
}
function button_styles() {
  return "px-[30px] h-[40px] rounded-md text-[13px] font-semibold shadow-md bg-gradient-to-r transition-all ";
}
function light_button_styles() {
  return button_styles() + "from-zinc-100 to-zinc-50 hover:from-zinc-200 hover:to-zinc-100";
}
function dark_button_styles() {
  return button_styles() + "from-zinc-800 to-zinc-900 hover:from-zinc-700 hover:to-zinc-800 text-white";
}
function view(model) {
  return initialize(
    default_config(),
    () => {
      return div(
        toList([class$("text-zinc-900 w-[60ch] mx-auto mt-48")]),
        toList([
          div(
            toList([class$("flex flex-col gap-3")]),
            toList([
              h1(
                toList([class$("text-5xl font-semibold text-center")]),
                toList([text2("Popcicle")])
              ),
              p(
                toList([class$("text-center text-lg mb-3")]),
                toList([
                  text2("Simple to use, styleable popovers for "),
                  span(
                    toList([]),
                    toList([
                      a(
                        toList([
                          class$(
                            "text-center underline text-violet-400 cursor-pointer"
                          ),
                          href("https://github.com/lustre-labs/lustre")
                        ]),
                        toList([text2("Lustre")])
                      )
                    ])
                  )
                ])
              ),
              div(
                toList([class$("flex gap-2 justify-center")]),
                toList([
                  dropdown_menu(
                    button(
                      toList([class$(dark_button_styles())]),
                      toList([text2("Open popover")])
                    ),
                    new BottomCenter(),
                    toList([]),
                    toList([
                      label(
                        toList([]),
                        toList([text2("My Account")])
                      ),
                      separator(toList([])),
                      item(
                        toList([]),
                        toList([
                          text2("Profile"),
                          shortcut(
                            toList([]),
                            toList([text2("\u21E7\u2318P")])
                          )
                        ])
                      ),
                      item(
                        toList([]),
                        toList([
                          text2("Billing"),
                          shortcut(
                            toList([]),
                            toList([text2("\u2318B")])
                          )
                        ])
                      ),
                      item(
                        toList([]),
                        toList([
                          text2("Settings"),
                          shortcut(
                            toList([]),
                            toList([text2("\u2318S")])
                          )
                        ])
                      ),
                      item(
                        toList([]),
                        toList([
                          text2("Keyboard shortcuts"),
                          shortcut(
                            toList([]),
                            toList([text2("\u2318K")])
                          )
                        ])
                      ),
                      separator(toList([])),
                      item(toList([]), toList([text2("Team")])),
                      dropdown_menu(
                        item(
                          toList([class$("justify-between")]),
                          toList([
                            text2("Invite users"),
                            chevron_right(toList([]))
                          ])
                        ),
                        new RightBottom(),
                        toList([class$("!w-28")]),
                        toList([
                          item(
                            toList([]),
                            toList([text2("Email")])
                          ),
                          item(
                            toList([]),
                            toList([text2("Message")])
                          ),
                          separator(toList([])),
                          item(
                            toList([]),
                            toList([text2("More...")])
                          )
                        ]),
                        new Hover()
                      ),
                      item(
                        toList([]),
                        toList([
                          text2("New Team"),
                          shortcut(
                            toList([]),
                            toList([text2("\u2318+T")])
                          )
                        ])
                      ),
                      item(toList([]), toList([text2("GitHub")])),
                      item(toList([]), toList([text2("Support")])),
                      item(
                        toList([disabled(true)]),
                        toList([text2("API")])
                      ),
                      separator(toList([])),
                      item(
                        toList([]),
                        toList([
                          text2("Log out"),
                          shortcut(
                            toList([]),
                            toList([text2("\u21E7\u2318Q")])
                          )
                        ])
                      )
                    ]),
                    new Click()
                  ),
                  a(
                    toList([href("https://github.com/dinkelspiel/popcicle")]),
                    toList([
                      button(
                        toList([class$(light_button_styles())]),
                        toList([text2("GitHub")])
                      )
                    ])
                  )
                ])
              ),
              a(
                toList([
                  class$(
                    "text-center underline text-zinc-400 text-sm cursor-pointer"
                  ),
                  href("https://hexdocs.pm/popcicle/")
                ]),
                toList([text2("Documentation")])
              )
            ])
          )
        ])
      );
    }
  );
}
function main() {
  let app = simple(init2, update, view);
  let $ = start2(app, "#app", 0);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "www",
      13,
      "main",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    );
  }
  return void 0;
}

// build/.lustre/entry.mjs
main();
