# O2/O3 JavaScript Generation Forms

This document describes the JavaScript forms to generate directly in elm-dev when porting the useful `elm-optimize-level-2` optimizations. The source optimizer rewrites JavaScript after `elm make --optimize`; elm-dev should instead emit these shapes from the compiler.

`-O2` means the default `elm-optimize-level-2` tool defaults. `-O3` means O2 plus the speed-focused record update specialization.

## Level Summary

`-O2` should include:

- Faster `F2` through `F9` and `A2` through `A9` wrappers.
- Raw-function aliases for wrapped functions, with exact-arity call sites emitted as direct calls.
- Unwrapped higher-order function variants when a function parameter is always called at one arity.
- Primitive equality emitted as JavaScript strict equality when safe.
- Normalized variant object shapes.
- Faster generated bodies for selected `elm/core` `List` functions.

`-O3` should include all O2 forms plus:

- Specialized record construction and record update forms using per-shape constructors and clone helpers.

The optimizer also contains experimental transforms that are not part of the CLI defaults. Those should not be included in the first port unless we explicitly decide otherwise.

## Faster Curried Wrappers

Current Elm output stores wrapper metadata in generic fields:

```js
function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
```

O2 uses arity-specific raw-function fields instead:

```js
function F2(fun) {
  var curried = function(a) { return function(b) { return fun(a,b); }; };
  curried.a2 = fun;
  return curried;
}

function A2(fun, a, b) {
  return fun.a2 ? fun.a2(a, b) : fun(a)(b);
}
```

Apply the same pattern for arities 2 through 9 with fields `a2` through `a9`. This removes the shared `F` helper and avoids checking a generic arity field on every full application.

## Direct Exact-Arity Calls

When a function is represented as `F2(function(a, b) { ... })`, O2 splits the raw function from the wrapped value:

```js
var add_fn = function(a, b) {
  return a + b;
};
var add = F2(add_fn);
```

An exact-arity call then becomes a raw JavaScript call:

```js
add_fn(x, y)
```

instead of:

```js
A2(add, x, y)
```

Generation rule:

- For generated functions of arity 2 through 9, emit a stable raw function binding and the wrapped value when both can be referenced.
- If a call target is known and the supplied argument count exactly matches the function arity, emit `raw(args...)`.
- If a call target is not known, is partially applied, or is applied at a different arity, keep the normal `A*` or curried call form.

The optimizer also handles aliases and some partial-application cases. The generation-time equivalent is to preserve enough function identity metadata in the optimized graph to know the raw binding and arity after links/aliases.

## Partial Application Completion

O2 recognizes cases where a partial application is later completed and avoids the wrapper path.

Post-transform shape:

```js
var addOne_a0 = 1;
var addOne = add(addOne_a0);

// later
add_fn(addOne_a0, value)
```

instead of:

```js
addOne(value)
```

Generation rule:

- When generating a partial application of a known arity 2 through 9 function, keep the applied arguments in bindings that can be reused.
- If a later call supplies exactly the remaining arguments, emit a direct raw call with saved arguments followed by new arguments.
- Otherwise keep the normal partial application value.

This is an optimization, not a semantic requirement. The first implementation can prioritize direct exact-arity calls and add completed partial applications after that.

## Higher-Order Unwrapped Variants

If a generated function accepts a function parameter and only invokes that parameter through `A*` at one arity, O2 creates a second variant that expects the raw JavaScript function directly.

Original shape:

```js
var apply2 = function(func, a, b) {
  return A2(func, a, b);
};
```

O2 shape:

```js
var apply2 = function(func, a, b) {
  return A2(func, a, b);
};

var apply2_unwrapped = function(func, a, b) {
  return func(a, b);
};
```

If a call passes a freshly wrapped lambda or a known split function:

```js
apply2_unwrapped(function(a, b) { return a + b; }, x, y)
apply2_unwrapped(add_fn, x, y)
```

instead of:

```js
apply2(F2(function(a, b) { return a + b; }), x, y)
apply2(add, x, y)
```

Generation rule:

- For each generated function, identify parameters that are only called as `A2` through `A9` at a single arity.
- Generate an `_unwrapped` variant where those calls are plain `param(args...)` calls.
- Use the `_unwrapped` variant only when the argument at that position is a raw function available at the same arity.
- Bail out if the parameter is called at multiple arities, called curried, stored where its wrapper semantics are needed, or passed to a function without a compatible unwrapped variant.

## Primitive Equality

O2 replaces `_Utils_eq(left, right)` with `left === right` when either side can be inferred to be a primitive literal or primitive numeric expression.

Generation rule:

- Emit `===` for `Basics.==` and `!==` for `/=` when the compared Elm type is definitely primitive: `Int`, `Float`, `String`, `Char`, or `Bool`.
- Keep `_Utils_eq` for structural types, functions, records, tuples, lists, custom types, JSON values of unknown shape, and any case where the type is not definitely primitive.

elm-dev already emits strict equality when one generated JavaScript operand is a literal. The O2 port should preserve that and can be more precise by using Elm type information instead of post-generation JavaScript inference.

## Normalized Variant Shapes

O2 pads variant objects so every constructor of a custom type has the same property shape. The enabled CLI behavior only includes built-in `List` and `Maybe`; the source has commented-out parsing support for wider project types.

Example:

```js
var $elm$core$Maybe$Just = function(a) {
  return { $: 0, a: a };
};

var $elm$core$Maybe$Nothing = { $: 1, a: null };
```

instead of:

```js
var $elm$core$Maybe$Nothing = { $: 1 };
```

Generation rule:

- For each constructor in a normalized type, emit the tag field plus every payload field used by the largest constructor in that type.
- Real payload fields keep their values. Missing fields are emitted as `null`.
- In dev mode the tag remains the constructor name string. In prod mode it remains the numeric constructor tag.

For a faithful first port, normalize `List` and `Maybe`. Since the compiler already knows all union declarations, a later implementation can extend this to all custom types if benchmarks justify it.

## Core List Function Bodies

O2 replaces selected `elm/core` `List` definitions with hand-written loop implementations that build result lists through a mutable tail pointer. These avoid extra reversals, intermediate lists, or callback wrapper calls where possible.

Included replacements:

- `List.all`
- `List.append`
- `List.concat`
- `List.concatMap`
- `List.filter`
- `List.indexedMap`
- `List.intersperse`
- `List.map`
- `List.partition`
- `List.take`
- `List.unzip`

Representative generated form:

```js
var $elm$core$List$map = F2(function(f, xs) {
  var tmp = _List_Cons(undefined, _List_Nil);
  var end = tmp;
  for (; xs.b; xs = xs.b) {
    var next = _List_Cons(f(xs.a), _List_Nil);
    end.b = next;
    end = next;
  }
  return tmp.b;
});
```

Generation rule:

- When emitting these core functions in O2/O3, use the loop bodies from the optimizer replacements instead of the current Elm definitions.
- Prefer direct callback calls for unary callback parameters when the replacement expects an unwrapped callback.
- Keep `A2` where the callback is genuinely binary, such as `List.indexedMap`.

String replacements are present in the optimizer repository but disabled in the CLI defaults. Do not include them in the initial O2 port.

## O3 Record Updates

O3 specializes record updates. It replaces general object-copying updates with per-record-shape constructors and clone methods.

For each record shape that participates in updates, O3 generates a constructor:

```js
function $$Record1(a,b,c) {
  this.a = a;
  this.b = b;
  this.c = c;
}

$$Record1.prototype.$c = function() {
  return new $$Record1(this.a, this.b, this.c);
};
```

Record literals of that shape become constructor calls:

```js
new $$Record1(a, b, c)
```

Single-use update shape:

```js
(function() {
  var $r = record.$c();
  $r.a = nextA;
  return $r;
}())
```

Repeated update shape:

```js
function $$update__a__b(obj,a,b) {
  var $r = obj.$c();
  $r.a = a;
  $r.b = b;
  return $r;
}

$$update__a__b(record, nextA, nextB)
```

Generation rule:

- Collect record shapes and update field sets during optimized-code generation.
- Use generated JavaScript field names after prod field shortening.
- Sort update field names deterministically before choosing a reusable helper name.
- Emit one constructor and `$c` clone method per record shape that can be updated.
- Emit a reusable update helper when the same update field set appears more than once.
- For single-use update field sets, emit the inline clone-and-assign expression.
- Keep normal object literals for record shapes that do not participate in O3 updates.

This optimization changes the runtime representation of affected records from plain object literals to objects with a prototype. It must remain gated behind `-O3` until we have compatibility and benchmark coverage.

## Not In The Initial Port

The optimizer repository contains additional transforms that are disabled in `toolDefaults` or marked as future work. These are out of scope for the first O2/O3 port:

- List literal inlining from `_List_fromArray([...])` to nested cons objects or `_List_cons` calls.
- `String.fromInt` / `String.fromFloat` inlining to `value + ''`.
- Arrow function conversion.
- Object spread or `Object.assign` record update variants.
- Object literal shorthand conversion.
- Unused local removal.
- Virtual DOM node replacement.
- String function replacements.
- V8 analysis/debug helpers.
- Tail recursion modulo cons.
- Constant lifting.
- Eta conversion.
- Tuple allocation removal for `case (a, b)`; the compiler already appears to avoid this in relevant cases.

## Source Map

Relevant optimizer source files reviewed:

- `src/types.ts` for `toolDefaults` and the O2/O3 split.
- `src/transform.ts` for transform order.
- `src/transforms/inlineWrappedFunctions.ts` for raw function splitting and exact-arity direct calls.
- `src/transforms/passUnwrappedFunctions.ts` for higher-order unwrapped variants.
- `src/transforms/inlineEquality.ts` for primitive equality inference.
- `src/transforms/variantShapes.ts` and `src/parsing/primitives.ts` for variant shape normalization.
- `src/transforms/recordUpdate.ts` for O3 record constructor/update specialization.
- `src/replacements/faster-function-wrappers/*` for `F*` and `A*` wrappers.
- `src/replacements/list/*` for enabled core `List` replacements.
