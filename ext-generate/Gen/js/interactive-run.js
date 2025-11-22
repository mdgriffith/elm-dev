var __commonJS = (cb, mod) => () => (mod || cb((mod = { exports: {} }).exports, mod), mod.exports);

// dist/interactive-generate.js
var require_interactive_generate = __commonJS((exports) => {
  (function(scope) {
    function F(arity, fun, wrapper) {
      wrapper.a = arity;
      wrapper.f = fun;
      return wrapper;
    }
    function F2(fun) {
      return F(2, fun, function(a) {
        return function(b) {
          return fun(a, b);
        };
      });
    }
    function F3(fun) {
      return F(3, fun, function(a) {
        return function(b) {
          return function(c) {
            return fun(a, b, c);
          };
        };
      });
    }
    function F4(fun) {
      return F(4, fun, function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return fun(a, b, c, d);
            };
          };
        };
      });
    }
    function F5(fun) {
      return F(5, fun, function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return fun(a, b, c, d, e);
              };
            };
          };
        };
      });
    }
    function F6(fun) {
      return F(6, fun, function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return fun(a, b, c, d, e, f);
                };
              };
            };
          };
        };
      });
    }
    function F7(fun) {
      return F(7, fun, function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return fun(a, b, c, d, e, f, g);
                  };
                };
              };
            };
          };
        };
      });
    }
    function F8(fun) {
      return F(8, fun, function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return function(h) {
                      return fun(a, b, c, d, e, f, g, h);
                    };
                  };
                };
              };
            };
          };
        };
      });
    }
    function F9(fun) {
      return F(9, fun, function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return function(h) {
                      return function(i) {
                        return fun(a, b, c, d, e, f, g, h, i);
                      };
                    };
                  };
                };
              };
            };
          };
        };
      });
    }
    function A2(fun, a, b) {
      return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
    }
    function A3(fun, a, b, c) {
      return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
    }
    function A4(fun, a, b, c, d) {
      return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
    }
    function A5(fun, a, b, c, d, e) {
      return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
    }
    function A6(fun, a, b, c, d, e, f) {
      return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
    }
    function A7(fun, a, b, c, d, e, f, g) {
      return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
    }
    function A8(fun, a, b, c, d, e, f, g, h) {
      return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
    }
    function A9(fun, a, b, c, d, e, f, g, h, i) {
      return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
    }
    var _JsArray_empty = [];
    function _JsArray_singleton(value) {
      return [value];
    }
    function _JsArray_length(array) {
      return array.length;
    }
    var _JsArray_initialize = F3(function(size, offset, func) {
      var result = new Array(size);
      for (var i = 0;i < size; i++) {
        result[i] = func(offset + i);
      }
      return result;
    });
    var _JsArray_initializeFromList = F2(function(max, ls) {
      var result = new Array(max);
      for (var i = 0;i < max && ls.b; i++) {
        result[i] = ls.a;
        ls = ls.b;
      }
      result.length = i;
      return _Utils_Tuple2(result, ls);
    });
    var _JsArray_unsafeGet = F2(function(index, array) {
      return array[index];
    });
    var _JsArray_unsafeSet = F3(function(index, value, array) {
      var length = array.length;
      var result = new Array(length);
      for (var i = 0;i < length; i++) {
        result[i] = array[i];
      }
      result[index] = value;
      return result;
    });
    var _JsArray_push = F2(function(value, array) {
      var length = array.length;
      var result = new Array(length + 1);
      for (var i = 0;i < length; i++) {
        result[i] = array[i];
      }
      result[length] = value;
      return result;
    });
    var _JsArray_foldl = F3(function(func, acc, array) {
      var length = array.length;
      for (var i = 0;i < length; i++) {
        acc = A2(func, array[i], acc);
      }
      return acc;
    });
    var _JsArray_foldr = F3(function(func, acc, array) {
      for (var i = array.length - 1;i >= 0; i--) {
        acc = A2(func, array[i], acc);
      }
      return acc;
    });
    var _JsArray_map = F2(function(func, array) {
      var length = array.length;
      var result = new Array(length);
      for (var i = 0;i < length; i++) {
        result[i] = func(array[i]);
      }
      return result;
    });
    var _JsArray_indexedMap = F3(function(func, offset, array) {
      var length = array.length;
      var result = new Array(length);
      for (var i = 0;i < length; i++) {
        result[i] = A2(func, offset + i, array[i]);
      }
      return result;
    });
    var _JsArray_slice = F3(function(from, to, array) {
      return array.slice(from, to);
    });
    var _JsArray_appendN = F3(function(n, dest, source) {
      var destLen = dest.length;
      var itemsToCopy = n - destLen;
      if (itemsToCopy > source.length) {
        itemsToCopy = source.length;
      }
      var size = destLen + itemsToCopy;
      var result = new Array(size);
      for (var i = 0;i < destLen; i++) {
        result[i] = dest[i];
      }
      for (var i = 0;i < itemsToCopy; i++) {
        result[i + destLen] = source[i];
      }
      return result;
    });
    var _Debug_log = F2(function(tag, value) {
      return value;
    });
    var _Debug_log_UNUSED = F2(function(tag, value) {
      console.log(tag + ": " + _Debug_toString(value));
      return value;
    });
    function _Debug_todo(moduleName, region) {
      return function(message) {
        _Debug_crash(8, moduleName, region, message);
      };
    }
    function _Debug_todoCase(moduleName, region, value) {
      return function(message) {
        _Debug_crash(9, moduleName, region, value, message);
      };
    }
    function _Debug_toString(value) {
      return "<internals>";
    }
    function _Debug_toString_UNUSED(value) {
      return _Debug_toAnsiString(false, value);
    }
    function _Debug_toAnsiString(ansi, value) {
      if (typeof value === "function") {
        return _Debug_internalColor(ansi, "<function>");
      }
      if (typeof value === "boolean") {
        return _Debug_ctorColor(ansi, value ? "True" : "False");
      }
      if (typeof value === "number") {
        return _Debug_numberColor(ansi, value + "");
      }
      if (value instanceof String) {
        return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
      }
      if (typeof value === "string") {
        return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
      }
      if (typeof value === "object" && "$" in value) {
        var tag = value.$;
        if (typeof tag === "number") {
          return _Debug_internalColor(ansi, "<internals>");
        }
        if (tag[0] === "#") {
          var output = [];
          for (var k in value) {
            if (k === "$")
              continue;
            output.push(_Debug_toAnsiString(ansi, value[k]));
          }
          return "(" + output.join(",") + ")";
        }
        if (tag === "Set_elm_builtin") {
          return _Debug_ctorColor(ansi, "Set") + _Debug_fadeColor(ansi, ".fromList") + " " + _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
        }
        if (tag === "RBNode_elm_builtin" || tag === "RBEmpty_elm_builtin") {
          return _Debug_ctorColor(ansi, "Dict") + _Debug_fadeColor(ansi, ".fromList") + " " + _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
        }
        if (tag === "Array_elm_builtin") {
          return _Debug_ctorColor(ansi, "Array") + _Debug_fadeColor(ansi, ".fromList") + " " + _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
        }
        if (tag === "::" || tag === "[]") {
          var output = "[";
          value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b);
          for (;value.b; value = value.b) {
            output += "," + _Debug_toAnsiString(ansi, value.a);
          }
          return output + "]";
        }
        var output = "";
        for (var i in value) {
          if (i === "$")
            continue;
          var str = _Debug_toAnsiString(ansi, value[i]);
          var c0 = str[0];
          var parenless = c0 === "{" || c0 === "(" || c0 === "[" || c0 === "<" || c0 === '"' || str.indexOf(" ") < 0;
          output += " " + (parenless ? str : "(" + str + ")");
        }
        return _Debug_ctorColor(ansi, tag) + output;
      }
      if (typeof DataView === "function" && value instanceof DataView) {
        return _Debug_stringColor(ansi, "<" + value.byteLength + " bytes>");
      }
      if (typeof File !== "undefined" && value instanceof File) {
        return _Debug_internalColor(ansi, "<" + value.name + ">");
      }
      if (typeof value === "object") {
        var output = [];
        for (var key in value) {
          var field = key[0] === "_" ? key.slice(1) : key;
          output.push(_Debug_fadeColor(ansi, field) + " = " + _Debug_toAnsiString(ansi, value[key]));
        }
        if (output.length === 0) {
          return "{}";
        }
        return "{ " + output.join(", ") + " }";
      }
      return _Debug_internalColor(ansi, "<internals>");
    }
    function _Debug_addSlashes(str, isChar) {
      var s = str.replace(/\\/g, "\\\\").replace(/\n/g, "\\n").replace(/\t/g, "\\t").replace(/\r/g, "\\r").replace(/\v/g, "\\v").replace(/\0/g, "\\0");
      if (isChar) {
        return s.replace(/\'/g, "\\'");
      } else {
        return s.replace(/\"/g, "\\\"");
      }
    }
    function _Debug_ctorColor(ansi, string) {
      return ansi ? "\x1B[96m" + string + "\x1B[0m" : string;
    }
    function _Debug_numberColor(ansi, string) {
      return ansi ? "\x1B[95m" + string + "\x1B[0m" : string;
    }
    function _Debug_stringColor(ansi, string) {
      return ansi ? "\x1B[93m" + string + "\x1B[0m" : string;
    }
    function _Debug_charColor(ansi, string) {
      return ansi ? "\x1B[92m" + string + "\x1B[0m" : string;
    }
    function _Debug_fadeColor(ansi, string) {
      return ansi ? "\x1B[37m" + string + "\x1B[0m" : string;
    }
    function _Debug_internalColor(ansi, string) {
      return ansi ? "\x1B[36m" + string + "\x1B[0m" : string;
    }
    function _Debug_toHexDigit(n) {
      return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
    }
    function _Debug_crash(identifier) {
      throw new Error("https://github.com/elm/core/blob/1.0.0/hints/" + identifier + ".md");
    }
    function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4) {
      switch (identifier) {
        case 0:
          throw new Error(`What node should I take over? In JavaScript I need something like:

    Elm.Main.init({
        node: document.getElementById("elm-node")
    })

You need to do this with any Browser.sandbox or Browser.element program.`);
        case 1:
          throw new Error(`Browser.application programs cannot handle URLs like this:

    ` + document.location.href + "\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.");
        case 2:
          var jsonErrorString = fact1;
          throw new Error(`Problem with the flags given to your Elm program on initialization.

` + jsonErrorString);
        case 3:
          var portName = fact1;
          throw new Error("There can only be one port named `" + portName + "`, but your program has multiple.");
        case 4:
          var portName = fact1;
          var problem = fact2;
          throw new Error("Trying to send an unexpected type of value through port `" + portName + "`:\n" + problem);
        case 5:
          throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');
        case 6:
          var moduleName = fact1;
          throw new Error("Your page is loading multiple Elm scripts with a module named " + moduleName + ". Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!");
        case 8:
          var moduleName = fact1;
          var region = fact2;
          var message = fact3;
          throw new Error("TODO in module `" + moduleName + "` " + _Debug_regionToString(region) + `

` + message);
        case 9:
          var moduleName = fact1;
          var region = fact2;
          var value = fact3;
          var message = fact4;
          throw new Error("TODO in module `" + moduleName + "` from the `case` expression " + _Debug_regionToString(region) + `

It received the following value:

    ` + _Debug_toString(value).replace(`
`, `
    `) + `

But the branch that handles it says:

    ` + message.replace(`
`, `
    `));
        case 10:
          throw new Error("Bug in https://github.com/elm/virtual-dom/issues");
        case 11:
          throw new Error("Cannot perform mod 0. Division by zero error.");
      }
    }
    function _Debug_regionToString(region) {
      if (region.z.ae === region.aT.ae) {
        return "on line " + region.z.ae;
      }
      return "on lines " + region.z.ae + " through " + region.aT.ae;
    }
    function _Utils_eq(x, y) {
      for (var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);isEqual && (pair = stack.pop()); isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)) {
      }
      return isEqual;
    }
    function _Utils_eqHelp(x, y, depth, stack) {
      if (x === y) {
        return true;
      }
      if (typeof x !== "object" || x === null || y === null) {
        typeof x === "function" && _Debug_crash(5);
        return false;
      }
      if (depth > 100) {
        stack.push(_Utils_Tuple2(x, y));
        return true;
      }
      if (x.$ < 0) {
        x = $elm$core$Dict$toList(x);
        y = $elm$core$Dict$toList(y);
      }
      for (var key in x) {
        if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack)) {
          return false;
        }
      }
      return true;
    }
    var _Utils_equal = F2(_Utils_eq);
    var _Utils_notEqual = F2(function(a, b) {
      return !_Utils_eq(a, b);
    });
    function _Utils_cmp(x, y, ord) {
      if (typeof x !== "object") {
        return x === y ? 0 : x < y ? -1 : 1;
      }
      if (typeof x.$ === "undefined") {
        return (ord = _Utils_cmp(x.a, y.a)) ? ord : (ord = _Utils_cmp(x.b, y.b)) ? ord : _Utils_cmp(x.c, y.c);
      }
      for (;x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {
      }
      return ord || (x.b ? 1 : y.b ? -1 : 0);
    }
    var _Utils_lt = F2(function(a, b) {
      return _Utils_cmp(a, b) < 0;
    });
    var _Utils_le = F2(function(a, b) {
      return _Utils_cmp(a, b) < 1;
    });
    var _Utils_gt = F2(function(a, b) {
      return _Utils_cmp(a, b) > 0;
    });
    var _Utils_ge = F2(function(a, b) {
      return _Utils_cmp(a, b) >= 0;
    });
    var _Utils_compare = F2(function(x, y) {
      var n = _Utils_cmp(x, y);
      return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
    });
    var _Utils_Tuple0 = 0;
    var _Utils_Tuple0_UNUSED = { $: "#0" };
    function _Utils_Tuple2(a, b) {
      return { a, b };
    }
    function _Utils_Tuple2_UNUSED(a, b) {
      return { $: "#2", a, b };
    }
    function _Utils_Tuple3(a, b, c) {
      return { a, b, c };
    }
    function _Utils_Tuple3_UNUSED(a, b, c) {
      return { $: "#3", a, b, c };
    }
    function _Utils_chr(c) {
      return c;
    }
    function _Utils_chr_UNUSED(c) {
      return new String(c);
    }
    function _Utils_update(oldRecord, updatedFields) {
      var newRecord = {};
      for (var key in oldRecord) {
        newRecord[key] = oldRecord[key];
      }
      for (var key in updatedFields) {
        newRecord[key] = updatedFields[key];
      }
      return newRecord;
    }
    var _Utils_append = F2(_Utils_ap);
    function _Utils_ap(xs, ys) {
      if (typeof xs === "string") {
        return xs + ys;
      }
      if (!xs.b) {
        return ys;
      }
      var root = _List_Cons(xs.a, ys);
      xs = xs.b;
      for (var curr = root;xs.b; xs = xs.b) {
        curr = curr.b = _List_Cons(xs.a, ys);
      }
      return root;
    }
    var _List_Nil = { $: 0 };
    var _List_Nil_UNUSED = { $: "[]" };
    function _List_Cons(hd, tl) {
      return { $: 1, a: hd, b: tl };
    }
    function _List_Cons_UNUSED(hd, tl) {
      return { $: "::", a: hd, b: tl };
    }
    var _List_cons = F2(_List_Cons);
    function _List_fromArray(arr) {
      var out = _List_Nil;
      for (var i = arr.length;i--; ) {
        out = _List_Cons(arr[i], out);
      }
      return out;
    }
    function _List_toArray(xs) {
      for (var out = [];xs.b; xs = xs.b) {
        out.push(xs.a);
      }
      return out;
    }
    var _List_map2 = F3(function(f, xs, ys) {
      for (var arr = [];xs.b && ys.b; xs = xs.b, ys = ys.b) {
        arr.push(A2(f, xs.a, ys.a));
      }
      return _List_fromArray(arr);
    });
    var _List_map3 = F4(function(f, xs, ys, zs) {
      for (var arr = [];xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) {
        arr.push(A3(f, xs.a, ys.a, zs.a));
      }
      return _List_fromArray(arr);
    });
    var _List_map4 = F5(function(f, ws, xs, ys, zs) {
      for (var arr = [];ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) {
        arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
      }
      return _List_fromArray(arr);
    });
    var _List_map5 = F6(function(f, vs, ws, xs, ys, zs) {
      for (var arr = [];vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) {
        arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
      }
      return _List_fromArray(arr);
    });
    var _List_sortBy = F2(function(f, xs) {
      return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
        return _Utils_cmp(f(a), f(b));
      }));
    });
    var _List_sortWith = F2(function(f, xs) {
      return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
        var ord = A2(f, a, b);
        return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
      }));
    });
    var _Basics_add = F2(function(a, b) {
      return a + b;
    });
    var _Basics_sub = F2(function(a, b) {
      return a - b;
    });
    var _Basics_mul = F2(function(a, b) {
      return a * b;
    });
    var _Basics_fdiv = F2(function(a, b) {
      return a / b;
    });
    var _Basics_idiv = F2(function(a, b) {
      return a / b | 0;
    });
    var _Basics_pow = F2(Math.pow);
    var _Basics_remainderBy = F2(function(b, a) {
      return a % b;
    });
    var _Basics_modBy = F2(function(modulus, x) {
      var answer = x % modulus;
      return modulus === 0 ? _Debug_crash(11) : answer > 0 && modulus < 0 || answer < 0 && modulus > 0 ? answer + modulus : answer;
    });
    var _Basics_pi = Math.PI;
    var _Basics_e = Math.E;
    var _Basics_cos = Math.cos;
    var _Basics_sin = Math.sin;
    var _Basics_tan = Math.tan;
    var _Basics_acos = Math.acos;
    var _Basics_asin = Math.asin;
    var _Basics_atan = Math.atan;
    var _Basics_atan2 = F2(Math.atan2);
    function _Basics_toFloat(x) {
      return x;
    }
    function _Basics_truncate(n) {
      return n | 0;
    }
    function _Basics_isInfinite(n) {
      return n === Infinity || n === -Infinity;
    }
    var _Basics_ceiling = Math.ceil;
    var _Basics_floor = Math.floor;
    var _Basics_round = Math.round;
    var _Basics_sqrt = Math.sqrt;
    var _Basics_log = Math.log;
    var _Basics_isNaN = isNaN;
    function _Basics_not(bool) {
      return !bool;
    }
    var _Basics_and = F2(function(a, b) {
      return a && b;
    });
    var _Basics_or = F2(function(a, b) {
      return a || b;
    });
    var _Basics_xor = F2(function(a, b) {
      return a !== b;
    });
    var _String_cons = F2(function(chr, str) {
      return chr + str;
    });
    function _String_uncons(string) {
      var word = string.charCodeAt(0);
      return !isNaN(word) ? $elm$core$Maybe$Just(55296 <= word && word <= 56319 ? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2)) : _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))) : $elm$core$Maybe$Nothing;
    }
    var _String_append = F2(function(a, b) {
      return a + b;
    });
    function _String_length(str) {
      return str.length;
    }
    var _String_map = F2(function(func, string) {
      var len = string.length;
      var array = new Array(len);
      var i = 0;
      while (i < len) {
        var word = string.charCodeAt(i);
        if (55296 <= word && word <= 56319) {
          array[i] = func(_Utils_chr(string[i] + string[i + 1]));
          i += 2;
          continue;
        }
        array[i] = func(_Utils_chr(string[i]));
        i++;
      }
      return array.join("");
    });
    var _String_filter = F2(function(isGood, str) {
      var arr = [];
      var len = str.length;
      var i = 0;
      while (i < len) {
        var char = str[i];
        var word = str.charCodeAt(i);
        i++;
        if (55296 <= word && word <= 56319) {
          char += str[i];
          i++;
        }
        if (isGood(_Utils_chr(char))) {
          arr.push(char);
        }
      }
      return arr.join("");
    });
    function _String_reverse(str) {
      var len = str.length;
      var arr = new Array(len);
      var i = 0;
      while (i < len) {
        var word = str.charCodeAt(i);
        if (55296 <= word && word <= 56319) {
          arr[len - i] = str[i + 1];
          i++;
          arr[len - i] = str[i - 1];
          i++;
        } else {
          arr[len - i] = str[i];
          i++;
        }
      }
      return arr.join("");
    }
    var _String_foldl = F3(function(func, state, string) {
      var len = string.length;
      var i = 0;
      while (i < len) {
        var char = string[i];
        var word = string.charCodeAt(i);
        i++;
        if (55296 <= word && word <= 56319) {
          char += string[i];
          i++;
        }
        state = A2(func, _Utils_chr(char), state);
      }
      return state;
    });
    var _String_foldr = F3(function(func, state, string) {
      var i = string.length;
      while (i--) {
        var char = string[i];
        var word = string.charCodeAt(i);
        if (56320 <= word && word <= 57343) {
          i--;
          char = string[i] + char;
        }
        state = A2(func, _Utils_chr(char), state);
      }
      return state;
    });
    var _String_split = F2(function(sep, str) {
      return str.split(sep);
    });
    var _String_join = F2(function(sep, strs) {
      return strs.join(sep);
    });
    var _String_slice = F3(function(start, end, str) {
      return str.slice(start, end);
    });
    function _String_trim(str) {
      return str.trim();
    }
    function _String_trimLeft(str) {
      return str.replace(/^\s+/, "");
    }
    function _String_trimRight(str) {
      return str.replace(/\s+$/, "");
    }
    function _String_words(str) {
      return _List_fromArray(str.trim().split(/\s+/g));
    }
    function _String_lines(str) {
      return _List_fromArray(str.split(/\r\n|\r|\n/g));
    }
    function _String_toUpper(str) {
      return str.toUpperCase();
    }
    function _String_toLower(str) {
      return str.toLowerCase();
    }
    var _String_any = F2(function(isGood, string) {
      var i = string.length;
      while (i--) {
        var char = string[i];
        var word = string.charCodeAt(i);
        if (56320 <= word && word <= 57343) {
          i--;
          char = string[i] + char;
        }
        if (isGood(_Utils_chr(char))) {
          return true;
        }
      }
      return false;
    });
    var _String_all = F2(function(isGood, string) {
      var i = string.length;
      while (i--) {
        var char = string[i];
        var word = string.charCodeAt(i);
        if (56320 <= word && word <= 57343) {
          i--;
          char = string[i] + char;
        }
        if (!isGood(_Utils_chr(char))) {
          return false;
        }
      }
      return true;
    });
    var _String_contains = F2(function(sub, str) {
      return str.indexOf(sub) > -1;
    });
    var _String_startsWith = F2(function(sub, str) {
      return str.indexOf(sub) === 0;
    });
    var _String_endsWith = F2(function(sub, str) {
      return str.length >= sub.length && str.lastIndexOf(sub) === str.length - sub.length;
    });
    var _String_indexes = F2(function(sub, str) {
      var subLen = sub.length;
      if (subLen < 1) {
        return _List_Nil;
      }
      var i = 0;
      var is = [];
      while ((i = str.indexOf(sub, i)) > -1) {
        is.push(i);
        i = i + subLen;
      }
      return _List_fromArray(is);
    });
    function _String_fromNumber(number) {
      return number + "";
    }
    function _String_toInt(str) {
      var total = 0;
      var code0 = str.charCodeAt(0);
      var start = code0 == 43 || code0 == 45 ? 1 : 0;
      for (var i = start;i < str.length; ++i) {
        var code = str.charCodeAt(i);
        if (code < 48 || 57 < code) {
          return $elm$core$Maybe$Nothing;
        }
        total = 10 * total + code - 48;
      }
      return i == start ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(code0 == 45 ? -total : total);
    }
    function _String_toFloat(s) {
      if (s.length === 0 || /[\sxbo]/.test(s)) {
        return $elm$core$Maybe$Nothing;
      }
      var n = +s;
      return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
    }
    function _String_fromList(chars) {
      return _List_toArray(chars).join("");
    }
    function _Char_toCode(char) {
      var code = char.charCodeAt(0);
      if (55296 <= code && code <= 56319) {
        return (code - 55296) * 1024 + char.charCodeAt(1) - 56320 + 65536;
      }
      return code;
    }
    function _Char_fromCode(code) {
      return _Utils_chr(code < 0 || 1114111 < code ? "�" : code <= 65535 ? String.fromCharCode(code) : (code -= 65536, String.fromCharCode(Math.floor(code / 1024) + 55296, code % 1024 + 56320)));
    }
    function _Char_toUpper(char) {
      return _Utils_chr(char.toUpperCase());
    }
    function _Char_toLower(char) {
      return _Utils_chr(char.toLowerCase());
    }
    function _Char_toLocaleUpper(char) {
      return _Utils_chr(char.toLocaleUpperCase());
    }
    function _Char_toLocaleLower(char) {
      return _Utils_chr(char.toLocaleLowerCase());
    }
    function _Json_succeed(msg) {
      return {
        $: 0,
        a: msg
      };
    }
    function _Json_fail(msg) {
      return {
        $: 1,
        a: msg
      };
    }
    function _Json_decodePrim(decoder) {
      return { $: 2, b: decoder };
    }
    var _Json_decodeInt = _Json_decodePrim(function(value) {
      return typeof value !== "number" ? _Json_expecting("an INT", value) : -2147483647 < value && value < 2147483647 && (value | 0) === value ? $elm$core$Result$Ok(value) : isFinite(value) && !(value % 1) ? $elm$core$Result$Ok(value) : _Json_expecting("an INT", value);
    });
    var _Json_decodeBool = _Json_decodePrim(function(value) {
      return typeof value === "boolean" ? $elm$core$Result$Ok(value) : _Json_expecting("a BOOL", value);
    });
    var _Json_decodeFloat = _Json_decodePrim(function(value) {
      return typeof value === "number" ? $elm$core$Result$Ok(value) : _Json_expecting("a FLOAT", value);
    });
    var _Json_decodeValue = _Json_decodePrim(function(value) {
      return $elm$core$Result$Ok(_Json_wrap(value));
    });
    var _Json_decodeString = _Json_decodePrim(function(value) {
      return typeof value === "string" ? $elm$core$Result$Ok(value) : value instanceof String ? $elm$core$Result$Ok(value + "") : _Json_expecting("a STRING", value);
    });
    function _Json_decodeList(decoder) {
      return { $: 3, b: decoder };
    }
    function _Json_decodeArray(decoder) {
      return { $: 4, b: decoder };
    }
    function _Json_decodeNull(value) {
      return { $: 5, c: value };
    }
    var _Json_decodeField = F2(function(field, decoder) {
      return {
        $: 6,
        d: field,
        b: decoder
      };
    });
    var _Json_decodeIndex = F2(function(index, decoder) {
      return {
        $: 7,
        e: index,
        b: decoder
      };
    });
    function _Json_decodeKeyValuePairs(decoder) {
      return {
        $: 8,
        b: decoder
      };
    }
    function _Json_mapMany(f, decoders) {
      return {
        $: 9,
        f,
        g: decoders
      };
    }
    var _Json_andThen = F2(function(callback, decoder) {
      return {
        $: 10,
        b: decoder,
        h: callback
      };
    });
    function _Json_oneOf(decoders) {
      return {
        $: 11,
        g: decoders
      };
    }
    var _Json_map1 = F2(function(f, d1) {
      return _Json_mapMany(f, [d1]);
    });
    var _Json_map2 = F3(function(f, d1, d2) {
      return _Json_mapMany(f, [d1, d2]);
    });
    var _Json_map3 = F4(function(f, d1, d2, d3) {
      return _Json_mapMany(f, [d1, d2, d3]);
    });
    var _Json_map4 = F5(function(f, d1, d2, d3, d4) {
      return _Json_mapMany(f, [d1, d2, d3, d4]);
    });
    var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5) {
      return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
    });
    var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6) {
      return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
    });
    var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7) {
      return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
    });
    var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8) {
      return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
    });
    var _Json_runOnString = F2(function(decoder, string) {
      try {
        var value = JSON.parse(string);
        return _Json_runHelp(decoder, value);
      } catch (e) {
        return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, "This is not valid JSON! " + e.message, _Json_wrap(string)));
      }
    });
    var _Json_run = F2(function(decoder, value) {
      return _Json_runHelp(decoder, _Json_unwrap(value));
    });
    function _Json_runHelp(decoder, value) {
      switch (decoder.$) {
        case 2:
          return decoder.b(value);
        case 5:
          return value === null ? $elm$core$Result$Ok(decoder.c) : _Json_expecting("null", value);
        case 3:
          if (!_Json_isArray(value)) {
            return _Json_expecting("a LIST", value);
          }
          return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);
        case 4:
          if (!_Json_isArray(value)) {
            return _Json_expecting("an ARRAY", value);
          }
          return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);
        case 6:
          var field = decoder.d;
          if (typeof value !== "object" || value === null || !(field in value)) {
            return _Json_expecting("an OBJECT with a field named `" + field + "`", value);
          }
          var result = _Json_runHelp(decoder.b, value[field]);
          return $elm$core$Result$isOk(result) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));
        case 7:
          var index = decoder.e;
          if (!_Json_isArray(value)) {
            return _Json_expecting("an ARRAY", value);
          }
          if (index >= value.length) {
            return _Json_expecting("a LONGER array. Need index " + index + " but only see " + value.length + " entries", value);
          }
          var result = _Json_runHelp(decoder.b, value[index]);
          return $elm$core$Result$isOk(result) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));
        case 8:
          if (typeof value !== "object" || value === null || _Json_isArray(value)) {
            return _Json_expecting("an OBJECT", value);
          }
          var keyValuePairs = _List_Nil;
          for (var key in value) {
            if (value.hasOwnProperty(key)) {
              var result = _Json_runHelp(decoder.b, value[key]);
              if (!$elm$core$Result$isOk(result)) {
                return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
              }
              keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
            }
          }
          return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));
        case 9:
          var answer = decoder.f;
          var decoders = decoder.g;
          for (var i = 0;i < decoders.length; i++) {
            var result = _Json_runHelp(decoders[i], value);
            if (!$elm$core$Result$isOk(result)) {
              return result;
            }
            answer = answer(result.a);
          }
          return $elm$core$Result$Ok(answer);
        case 10:
          var result = _Json_runHelp(decoder.b, value);
          return !$elm$core$Result$isOk(result) ? result : _Json_runHelp(decoder.h(result.a), value);
        case 11:
          var errors = _List_Nil;
          for (var temp = decoder.g;temp.b; temp = temp.b) {
            var result = _Json_runHelp(temp.a, value);
            if ($elm$core$Result$isOk(result)) {
              return result;
            }
            errors = _List_Cons(result.a, errors);
          }
          return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));
        case 1:
          return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));
        case 0:
          return $elm$core$Result$Ok(decoder.a);
      }
    }
    function _Json_runArrayDecoder(decoder, value, toElmValue) {
      var len = value.length;
      var array = new Array(len);
      for (var i = 0;i < len; i++) {
        var result = _Json_runHelp(decoder, value[i]);
        if (!$elm$core$Result$isOk(result)) {
          return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
        }
        array[i] = result.a;
      }
      return $elm$core$Result$Ok(toElmValue(array));
    }
    function _Json_isArray(value) {
      return Array.isArray(value) || typeof FileList !== "undefined" && value instanceof FileList;
    }
    function _Json_toElmArray(array) {
      return A2($elm$core$Array$initialize, array.length, function(i) {
        return array[i];
      });
    }
    function _Json_expecting(type, value) {
      return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, "Expecting " + type, _Json_wrap(value)));
    }
    function _Json_equality(x, y) {
      if (x === y) {
        return true;
      }
      if (x.$ !== y.$) {
        return false;
      }
      switch (x.$) {
        case 0:
        case 1:
          return x.a === y.a;
        case 2:
          return x.b === y.b;
        case 5:
          return x.c === y.c;
        case 3:
        case 4:
        case 8:
          return _Json_equality(x.b, y.b);
        case 6:
          return x.d === y.d && _Json_equality(x.b, y.b);
        case 7:
          return x.e === y.e && _Json_equality(x.b, y.b);
        case 9:
          return x.f === y.f && _Json_listEquality(x.g, y.g);
        case 10:
          return x.h === y.h && _Json_equality(x.b, y.b);
        case 11:
          return _Json_listEquality(x.g, y.g);
      }
    }
    function _Json_listEquality(aDecoders, bDecoders) {
      var len = aDecoders.length;
      if (len !== bDecoders.length) {
        return false;
      }
      for (var i = 0;i < len; i++) {
        if (!_Json_equality(aDecoders[i], bDecoders[i])) {
          return false;
        }
      }
      return true;
    }
    var _Json_encode = F2(function(indentLevel, value) {
      return JSON.stringify(_Json_unwrap(value), null, indentLevel) + "";
    });
    function _Json_wrap_UNUSED(value) {
      return { $: 0, a: value };
    }
    function _Json_unwrap_UNUSED(value) {
      return value.a;
    }
    function _Json_wrap(value) {
      return value;
    }
    function _Json_unwrap(value) {
      return value;
    }
    function _Json_emptyArray() {
      return [];
    }
    function _Json_emptyObject() {
      return {};
    }
    var _Json_addField = F3(function(key, value, object) {
      object[key] = _Json_unwrap(value);
      return object;
    });
    function _Json_addEntry(func) {
      return F2(function(entry, array) {
        array.push(_Json_unwrap(func(entry)));
        return array;
      });
    }
    var _Json_encodeNull = _Json_wrap(null);
    var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString) {
      var smallLength = smallString.length;
      var isGood = offset + smallLength <= bigString.length;
      for (var i = 0;isGood && i < smallLength; ) {
        var code = bigString.charCodeAt(offset);
        isGood = smallString[i++] === bigString[offset++] && (code === 10 ? (row++, col = 1) : (col++, (code & 63488) === 55296 ? smallString[i++] === bigString[offset++] : 1));
      }
      return _Utils_Tuple3(isGood ? offset : -1, row, col);
    });
    var _Parser_isSubChar = F3(function(predicate, offset, string) {
      return string.length <= offset ? -1 : (string.charCodeAt(offset) & 63488) === 55296 ? predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1 : predicate(_Utils_chr(string[offset])) ? string[offset] === `
` ? -2 : offset + 1 : -1;
    });
    var _Parser_isAsciiCode = F3(function(code, offset, string) {
      return string.charCodeAt(offset) === code;
    });
    var _Parser_chompBase10 = F2(function(offset, string) {
      for (;offset < string.length; offset++) {
        var code = string.charCodeAt(offset);
        if (code < 48 || 57 < code) {
          return offset;
        }
      }
      return offset;
    });
    var _Parser_consumeBase = F3(function(base, offset, string) {
      for (var total = 0;offset < string.length; offset++) {
        var digit = string.charCodeAt(offset) - 48;
        if (digit < 0 || base <= digit)
          break;
        total = base * total + digit;
      }
      return _Utils_Tuple2(offset, total);
    });
    var _Parser_consumeBase16 = F2(function(offset, string) {
      for (var total = 0;offset < string.length; offset++) {
        var code = string.charCodeAt(offset);
        if (48 <= code && code <= 57) {
          total = 16 * total + code - 48;
        } else if (65 <= code && code <= 70) {
          total = 16 * total + code - 55;
        } else if (97 <= code && code <= 102) {
          total = 16 * total + code - 87;
        } else {
          break;
        }
      }
      return _Utils_Tuple2(offset, total);
    });
    var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString) {
      var newOffset = bigString.indexOf(smallString, offset);
      var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;
      while (offset < target) {
        var code = bigString.charCodeAt(offset++);
        code === 10 ? (col = 1, row++) : (col++, (code & 63488) === 55296 && offset++);
      }
      return _Utils_Tuple3(newOffset, row, col);
    });
    function _Scheduler_succeed(value) {
      return {
        $: 0,
        a: value
      };
    }
    function _Scheduler_fail(error) {
      return {
        $: 1,
        a: error
      };
    }
    function _Scheduler_binding(callback) {
      return {
        $: 2,
        b: callback,
        c: null
      };
    }
    var _Scheduler_andThen = F2(function(callback, task) {
      return {
        $: 3,
        b: callback,
        d: task
      };
    });
    var _Scheduler_onError = F2(function(callback, task) {
      return {
        $: 4,
        b: callback,
        d: task
      };
    });
    function _Scheduler_receive(callback) {
      return {
        $: 5,
        b: callback
      };
    }
    var _Scheduler_guid = 0;
    function _Scheduler_rawSpawn(task) {
      var proc = {
        $: 0,
        e: _Scheduler_guid++,
        f: task,
        g: null,
        h: []
      };
      _Scheduler_enqueue(proc);
      return proc;
    }
    function _Scheduler_spawn(task) {
      return _Scheduler_binding(function(callback) {
        callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
      });
    }
    function _Scheduler_rawSend(proc, msg) {
      proc.h.push(msg);
      _Scheduler_enqueue(proc);
    }
    var _Scheduler_send = F2(function(proc, msg) {
      return _Scheduler_binding(function(callback) {
        _Scheduler_rawSend(proc, msg);
        callback(_Scheduler_succeed(_Utils_Tuple0));
      });
    });
    function _Scheduler_kill(proc) {
      return _Scheduler_binding(function(callback) {
        var task = proc.f;
        if (task.$ === 2 && task.c) {
          task.c();
        }
        proc.f = null;
        callback(_Scheduler_succeed(_Utils_Tuple0));
      });
    }
    var _Scheduler_working = false;
    var _Scheduler_queue = [];
    function _Scheduler_enqueue(proc) {
      _Scheduler_queue.push(proc);
      if (_Scheduler_working) {
        return;
      }
      _Scheduler_working = true;
      while (proc = _Scheduler_queue.shift()) {
        _Scheduler_step(proc);
      }
      _Scheduler_working = false;
    }
    function _Scheduler_step(proc) {
      while (proc.f) {
        var rootTag = proc.f.$;
        if (rootTag === 0 || rootTag === 1) {
          while (proc.g && proc.g.$ !== rootTag) {
            proc.g = proc.g.i;
          }
          if (!proc.g) {
            return;
          }
          proc.f = proc.g.b(proc.f.a);
          proc.g = proc.g.i;
        } else if (rootTag === 2) {
          proc.f.c = proc.f.b(function(newRoot) {
            proc.f = newRoot;
            _Scheduler_enqueue(proc);
          });
          return;
        } else if (rootTag === 5) {
          if (proc.h.length === 0) {
            return;
          }
          proc.f = proc.f.b(proc.h.shift());
        } else {
          proc.g = {
            $: rootTag === 3 ? 0 : 1,
            b: proc.f.b,
            i: proc.g
          };
          proc.f = proc.f.d;
        }
      }
    }
    function _Process_sleep(time) {
      return _Scheduler_binding(function(callback) {
        var id = setTimeout(function() {
          callback(_Scheduler_succeed(_Utils_Tuple0));
        }, time);
        return function() {
          clearTimeout(id);
        };
      });
    }
    var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args) {
      return _Platform_initialize(flagDecoder, args, impl.J, impl.aJ, impl.bk, function() {
        return function() {
        };
      });
    });
    function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder) {
      var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args["flags"] : undefined));
      $elm$core$Result$isOk(result) || _Debug_crash(2);
      var managers = {};
      var initPair = init(result.a);
      var model = initPair.a;
      var stepper = stepperBuilder(sendToApp, model);
      var ports = _Platform_setupEffects(managers, sendToApp);
      function sendToApp(msg, viewMetadata) {
        var pair = A2(update, msg, model);
        stepper(model = pair.a, viewMetadata);
        _Platform_enqueueEffects(managers, pair.b, subscriptions(model));
      }
      _Platform_enqueueEffects(managers, initPair.b, subscriptions(model));
      return ports ? { ports } : {};
    }
    var _Platform_preload;
    function _Platform_registerPreload(url) {
      _Platform_preload.add(url);
    }
    var _Platform_effectManagers = {};
    function _Platform_setupEffects(managers, sendToApp) {
      var ports;
      for (var key in _Platform_effectManagers) {
        var manager = _Platform_effectManagers[key];
        if (manager.a) {
          ports = ports || {};
          ports[key] = manager.a(key, sendToApp);
        }
        managers[key] = _Platform_instantiateManager(manager, sendToApp);
      }
      return ports;
    }
    function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap) {
      return {
        b: init,
        c: onEffects,
        d: onSelfMsg,
        e: cmdMap,
        f: subMap
      };
    }
    function _Platform_instantiateManager(info, sendToApp) {
      var router = {
        g: sendToApp,
        h: undefined
      };
      var onEffects = info.c;
      var onSelfMsg = info.d;
      var cmdMap = info.e;
      var subMap = info.f;
      function loop(state) {
        return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg) {
          var value = msg.a;
          if (msg.$ === 0) {
            return A3(onSelfMsg, router, value, state);
          }
          return cmdMap && subMap ? A4(onEffects, router, value.i, value.j, state) : A3(onEffects, router, cmdMap ? value.i : value.j, state);
        }));
      }
      return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
    }
    var _Platform_sendToApp = F2(function(router, msg) {
      return _Scheduler_binding(function(callback) {
        router.g(msg);
        callback(_Scheduler_succeed(_Utils_Tuple0));
      });
    });
    var _Platform_sendToSelf = F2(function(router, msg) {
      return A2(_Scheduler_send, router.h, {
        $: 0,
        a: msg
      });
    });
    function _Platform_leaf(home) {
      return function(value) {
        return {
          $: 1,
          k: home,
          l: value
        };
      };
    }
    function _Platform_batch(list) {
      return {
        $: 2,
        m: list
      };
    }
    var _Platform_map = F2(function(tagger, bag) {
      return {
        $: 3,
        n: tagger,
        o: bag
      };
    });
    var _Platform_effectsQueue = [];
    var _Platform_effectsActive = false;
    function _Platform_enqueueEffects(managers, cmdBag, subBag) {
      _Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });
      if (_Platform_effectsActive)
        return;
      _Platform_effectsActive = true;
      for (var fx;fx = _Platform_effectsQueue.shift(); ) {
        _Platform_dispatchEffects(fx.p, fx.q, fx.r);
      }
      _Platform_effectsActive = false;
    }
    function _Platform_dispatchEffects(managers, cmdBag, subBag) {
      var effectsDict = {};
      _Platform_gatherEffects(true, cmdBag, effectsDict, null);
      _Platform_gatherEffects(false, subBag, effectsDict, null);
      for (var home in managers) {
        _Scheduler_rawSend(managers[home], {
          $: "fx",
          a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
        });
      }
    }
    function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers) {
      switch (bag.$) {
        case 1:
          var home = bag.k;
          var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
          effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
          return;
        case 2:
          for (var list = bag.m;list.b; list = list.b) {
            _Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
          }
          return;
        case 3:
          _Platform_gatherEffects(isCmd, bag.o, effectsDict, {
            s: bag.n,
            t: taggers
          });
          return;
      }
    }
    function _Platform_toEffect(isCmd, home, taggers, value) {
      function applyTaggers(x) {
        for (var temp = taggers;temp; temp = temp.t) {
          x = temp.s(x);
        }
        return x;
      }
      var map = isCmd ? _Platform_effectManagers[home].e : _Platform_effectManagers[home].f;
      return A2(map, applyTaggers, value);
    }
    function _Platform_insert(isCmd, newEffect, effects) {
      effects = effects || { i: _List_Nil, j: _List_Nil };
      isCmd ? effects.i = _List_Cons(newEffect, effects.i) : effects.j = _List_Cons(newEffect, effects.j);
      return effects;
    }
    function _Platform_checkPortName(name) {
      if (_Platform_effectManagers[name]) {
        _Debug_crash(3, name);
      }
    }
    function _Platform_outgoingPort(name, converter) {
      _Platform_checkPortName(name);
      _Platform_effectManagers[name] = {
        e: _Platform_outgoingPortMap,
        u: converter,
        a: _Platform_setupOutgoingPort
      };
      return _Platform_leaf(name);
    }
    var _Platform_outgoingPortMap = F2(function(tagger, value) {
      return value;
    });
    function _Platform_setupOutgoingPort(name) {
      var subs = [];
      var converter = _Platform_effectManagers[name].u;
      var init = _Process_sleep(0);
      _Platform_effectManagers[name].b = init;
      _Platform_effectManagers[name].c = F3(function(router, cmdList, state) {
        for (;cmdList.b; cmdList = cmdList.b) {
          var currentSubs = subs;
          var value = _Json_unwrap(converter(cmdList.a));
          for (var i = 0;i < currentSubs.length; i++) {
            currentSubs[i](value);
          }
        }
        return init;
      });
      function subscribe(callback) {
        subs.push(callback);
      }
      function unsubscribe(callback) {
        subs = subs.slice();
        var index = subs.indexOf(callback);
        if (index >= 0) {
          subs.splice(index, 1);
        }
      }
      return {
        subscribe,
        unsubscribe
      };
    }
    function _Platform_incomingPort(name, converter) {
      _Platform_checkPortName(name);
      _Platform_effectManagers[name] = {
        f: _Platform_incomingPortMap,
        u: converter,
        a: _Platform_setupIncomingPort
      };
      return _Platform_leaf(name);
    }
    var _Platform_incomingPortMap = F2(function(tagger, finalTagger) {
      return function(value) {
        return tagger(finalTagger(value));
      };
    });
    function _Platform_setupIncomingPort(name, sendToApp) {
      var subs = _List_Nil;
      var converter = _Platform_effectManagers[name].u;
      var init = _Scheduler_succeed(null);
      _Platform_effectManagers[name].b = init;
      _Platform_effectManagers[name].c = F3(function(router, subList, state) {
        subs = subList;
        return init;
      });
      function send(incomingValue) {
        var result = A2(_Json_run, converter, _Json_wrap(incomingValue));
        $elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);
        var value = result.a;
        for (var temp = subs;temp.b; temp = temp.b) {
          sendToApp(temp.a(value));
        }
      }
      return { send };
    }
    function _Platform_export(exports2) {
      scope["Elm"] ? _Platform_mergeExportsProd(scope["Elm"], exports2) : scope["Elm"] = exports2;
    }
    function _Platform_mergeExportsProd(obj, exports2) {
      for (var name in exports2) {
        name in obj ? name == "init" ? _Debug_crash(6) : _Platform_mergeExportsProd(obj[name], exports2[name]) : obj[name] = exports2[name];
      }
    }
    function _Platform_export_UNUSED(exports2) {
      scope["Elm"] ? _Platform_mergeExportsDebug("Elm", scope["Elm"], exports2) : scope["Elm"] = exports2;
    }
    function _Platform_mergeExportsDebug(moduleName, obj, exports2) {
      for (var name in exports2) {
        name in obj ? name == "init" ? _Debug_crash(6, moduleName) : _Platform_mergeExportsDebug(moduleName + "." + name, obj[name], exports2[name]) : obj[name] = exports2[name];
      }
    }
    var _Bitwise_and = F2(function(a, b) {
      return a & b;
    });
    var _Bitwise_or = F2(function(a, b) {
      return a | b;
    });
    var _Bitwise_xor = F2(function(a, b) {
      return a ^ b;
    });
    function _Bitwise_complement(a) {
      return ~a;
    }
    var _Bitwise_shiftLeftBy = F2(function(offset, a) {
      return a << offset;
    });
    var _Bitwise_shiftRightBy = F2(function(offset, a) {
      return a >> offset;
    });
    var _Bitwise_shiftRightZfBy = F2(function(offset, a) {
      return a >>> offset;
    });
    var $elm$core$List$cons = _List_cons;
    var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
    var $elm$core$Array$foldr = F3(function(func, baseCase, _v0) {
      var tree = _v0.c;
      var tail = _v0.d;
      var helper = F2(function(node, acc) {
        if (!node.$) {
          var subTree = node.a;
          return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
        } else {
          var values = node.a;
          return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
        }
      });
      return A3($elm$core$Elm$JsArray$foldr, helper, A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail), tree);
    });
    var $elm$core$Array$toList = function(array) {
      return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
    };
    var $elm$core$Dict$foldr = F3(function(func, acc, t) {
      foldr:
        while (true) {
          if (t.$ === -2) {
            return acc;
          } else {
            var key = t.b;
            var value = t.c;
            var left = t.d;
            var right = t.e;
            var $temp$func = func, $temp$acc = A3(func, key, value, A3($elm$core$Dict$foldr, func, acc, right)), $temp$t = left;
            func = $temp$func;
            acc = $temp$acc;
            t = $temp$t;
            continue foldr;
          }
        }
    });
    var $elm$core$Dict$toList = function(dict) {
      return A3($elm$core$Dict$foldr, F3(function(key, value, list) {
        return A2($elm$core$List$cons, _Utils_Tuple2(key, value), list);
      }), _List_Nil, dict);
    };
    var $elm$core$Dict$keys = function(dict) {
      return A3($elm$core$Dict$foldr, F3(function(key, value, keyList) {
        return A2($elm$core$List$cons, key, keyList);
      }), _List_Nil, dict);
    };
    var $elm$core$Set$toList = function(_v0) {
      var dict = _v0;
      return $elm$core$Dict$keys(dict);
    };
    var $elm$core$Basics$EQ = 1;
    var $elm$core$Basics$GT = 2;
    var $elm$core$Basics$LT = 0;
    var $elm$core$Basics$add = _Basics_add;
    var $elm$core$List$foldl = F3(function(func, acc, list) {
      foldl:
        while (true) {
          if (!list.b) {
            return acc;
          } else {
            var x = list.a;
            var xs = list.b;
            var $temp$func = func, $temp$acc = A2(func, x, acc), $temp$list = xs;
            func = $temp$func;
            acc = $temp$acc;
            list = $temp$list;
            continue foldl;
          }
        }
    });
    var $elm$core$Basics$gt = _Utils_gt;
    var $elm$core$List$reverse = function(list) {
      return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
    };
    var $elm$core$List$foldrHelper = F4(function(fn, acc, ctr, ls) {
      if (!ls.b) {
        return acc;
      } else {
        var a = ls.a;
        var r1 = ls.b;
        if (!r1.b) {
          return A2(fn, a, acc);
        } else {
          var b = r1.a;
          var r2 = r1.b;
          if (!r2.b) {
            return A2(fn, a, A2(fn, b, acc));
          } else {
            var c = r2.a;
            var r3 = r2.b;
            if (!r3.b) {
              return A2(fn, a, A2(fn, b, A2(fn, c, acc)));
            } else {
              var d = r3.a;
              var r4 = r3.b;
              var res = ctr > 500 ? A3($elm$core$List$foldl, fn, acc, $elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
              return A2(fn, a, A2(fn, b, A2(fn, c, A2(fn, d, res))));
            }
          }
        }
      }
    });
    var $elm$core$List$foldr = F3(function(fn, acc, ls) {
      return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
    });
    var $elm$core$List$append = F2(function(xs, ys) {
      if (!ys.b) {
        return xs;
      } else {
        return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
      }
    });
    var $elm$core$List$concat = function(lists) {
      return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
    };
    var $elm$core$List$map = F2(function(f, xs) {
      return A3($elm$core$List$foldr, F2(function(x, acc) {
        return A2($elm$core$List$cons, f(x), acc);
      }), _List_Nil, xs);
    });
    var $elm$core$List$concatMap = F2(function(f, list) {
      return $elm$core$List$concat(A2($elm$core$List$map, f, list));
    });
    var $author$project$Options$Options = F2(function(project, viewers) {
      return { jh: project, jD: viewers };
    });
    var $elm$project_metadata_utils$Elm$Docs$Module = F6(function(name, comment, unions, aliases, values, binops) {
      return { ib: aliases, b3: binops, cp: comment, ag: name, am: unions, jC: values };
    });
    var $elm$project_metadata_utils$Elm$Docs$Alias = F4(function(name, comment, args, tipe) {
      return { G: args, cp: comment, ag: name, ak: tipe };
    });
    var $elm$core$Result$Err = function(a) {
      return { $: 1, a };
    };
    var $elm$json$Json$Decode$Failure = F2(function(a, b) {
      return { $: 3, a, b };
    });
    var $elm$json$Json$Decode$Field = F2(function(a, b) {
      return { $: 0, a, b };
    });
    var $elm$json$Json$Decode$Index = F2(function(a, b) {
      return { $: 1, a, b };
    });
    var $elm$core$Result$Ok = function(a) {
      return { $: 0, a };
    };
    var $elm$json$Json$Decode$OneOf = function(a) {
      return { $: 2, a };
    };
    var $elm$core$Basics$False = 1;
    var $elm$core$Maybe$Just = function(a) {
      return { $: 0, a };
    };
    var $elm$core$Maybe$Nothing = { $: 1 };
    var $elm$core$String$all = _String_all;
    var $elm$core$Basics$and = _Basics_and;
    var $elm$core$Basics$append = _Utils_append;
    var $elm$json$Json$Encode$encode = _Json_encode;
    var $elm$core$String$fromInt = _String_fromNumber;
    var $elm$core$String$join = F2(function(sep, chunks) {
      return A2(_String_join, sep, _List_toArray(chunks));
    });
    var $elm$core$String$split = F2(function(sep, string) {
      return _List_fromArray(A2(_String_split, sep, string));
    });
    var $elm$json$Json$Decode$indent = function(str) {
      return A2($elm$core$String$join, `
    `, A2($elm$core$String$split, `
`, str));
    };
    var $elm$core$List$length = function(xs) {
      return A3($elm$core$List$foldl, F2(function(_v0, i) {
        return i + 1;
      }), 0, xs);
    };
    var $elm$core$List$map2 = _List_map2;
    var $elm$core$Basics$le = _Utils_le;
    var $elm$core$Basics$sub = _Basics_sub;
    var $elm$core$List$rangeHelp = F3(function(lo, hi, list) {
      rangeHelp:
        while (true) {
          if (_Utils_cmp(lo, hi) < 1) {
            var $temp$lo = lo, $temp$hi = hi - 1, $temp$list = A2($elm$core$List$cons, hi, list);
            lo = $temp$lo;
            hi = $temp$hi;
            list = $temp$list;
            continue rangeHelp;
          } else {
            return list;
          }
        }
    });
    var $elm$core$List$range = F2(function(lo, hi) {
      return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
    });
    var $elm$core$List$indexedMap = F2(function(f, xs) {
      return A3($elm$core$List$map2, f, A2($elm$core$List$range, 0, $elm$core$List$length(xs) - 1), xs);
    });
    var $elm$core$Char$toCode = _Char_toCode;
    var $elm$core$Char$isLower = function(_char) {
      var code = $elm$core$Char$toCode(_char);
      return 97 <= code && code <= 122;
    };
    var $elm$core$Char$isUpper = function(_char) {
      var code = $elm$core$Char$toCode(_char);
      return code <= 90 && 65 <= code;
    };
    var $elm$core$Basics$or = _Basics_or;
    var $elm$core$Char$isAlpha = function(_char) {
      return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
    };
    var $elm$core$Char$isDigit = function(_char) {
      var code = $elm$core$Char$toCode(_char);
      return code <= 57 && 48 <= code;
    };
    var $elm$core$Char$isAlphaNum = function(_char) {
      return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
    };
    var $elm$core$String$uncons = _String_uncons;
    var $elm$json$Json$Decode$errorOneOf = F2(function(i, error) {
      return `

(` + ($elm$core$String$fromInt(i + 1) + (") " + $elm$json$Json$Decode$indent($elm$json$Json$Decode$errorToString(error))));
    });
    var $elm$json$Json$Decode$errorToString = function(error) {
      return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
    };
    var $elm$json$Json$Decode$errorToStringHelp = F2(function(error, context) {
      errorToStringHelp:
        while (true) {
          switch (error.$) {
            case 0:
              var f = error.a;
              var err = error.b;
              var isSimple = function() {
                var _v1 = $elm$core$String$uncons(f);
                if (_v1.$ === 1) {
                  return false;
                } else {
                  var _v2 = _v1.a;
                  var _char = _v2.a;
                  var rest = _v2.b;
                  return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
                }
              }();
              var fieldName = isSimple ? "." + f : "['" + (f + "']");
              var $temp$error = err, $temp$context = A2($elm$core$List$cons, fieldName, context);
              error = $temp$error;
              context = $temp$context;
              continue errorToStringHelp;
            case 1:
              var i = error.a;
              var err = error.b;
              var indexName = "[" + ($elm$core$String$fromInt(i) + "]");
              var $temp$error = err, $temp$context = A2($elm$core$List$cons, indexName, context);
              error = $temp$error;
              context = $temp$context;
              continue errorToStringHelp;
            case 2:
              var errors = error.a;
              if (!errors.b) {
                return "Ran into a Json.Decode.oneOf with no possibilities" + function() {
                  if (!context.b) {
                    return "!";
                  } else {
                    return " at json" + A2($elm$core$String$join, "", $elm$core$List$reverse(context));
                  }
                }();
              } else {
                if (!errors.b.b) {
                  var err = errors.a;
                  var $temp$error = err, $temp$context = context;
                  error = $temp$error;
                  context = $temp$context;
                  continue errorToStringHelp;
                } else {
                  var starter = function() {
                    if (!context.b) {
                      return "Json.Decode.oneOf";
                    } else {
                      return "The Json.Decode.oneOf at json" + A2($elm$core$String$join, "", $elm$core$List$reverse(context));
                    }
                  }();
                  var introduction = starter + (" failed in the following " + ($elm$core$String$fromInt($elm$core$List$length(errors)) + " ways:"));
                  return A2($elm$core$String$join, `

`, A2($elm$core$List$cons, introduction, A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
                }
              }
            default:
              var msg = error.a;
              var json = error.b;
              var introduction = function() {
                if (!context.b) {
                  return `Problem with the given value:

`;
                } else {
                  return "Problem with the value at json" + (A2($elm$core$String$join, "", $elm$core$List$reverse(context)) + `:

    `);
                }
              }();
              return introduction + ($elm$json$Json$Decode$indent(A2($elm$json$Json$Encode$encode, 4, json)) + (`

` + msg));
          }
        }
    });
    var $elm$core$Array$branchFactor = 32;
    var $elm$core$Array$Array_elm_builtin = F4(function(a, b, c, d) {
      return { $: 0, a, b, c, d };
    });
    var $elm$core$Elm$JsArray$empty = _JsArray_empty;
    var $elm$core$Basics$ceiling = _Basics_ceiling;
    var $elm$core$Basics$fdiv = _Basics_fdiv;
    var $elm$core$Basics$logBase = F2(function(base, number) {
      return _Basics_log(number) / _Basics_log(base);
    });
    var $elm$core$Basics$toFloat = _Basics_toFloat;
    var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
    var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
    var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
    var $elm$core$Array$Leaf = function(a) {
      return { $: 1, a };
    };
    var $elm$core$Basics$apL = F2(function(f, x) {
      return f(x);
    });
    var $elm$core$Basics$apR = F2(function(x, f) {
      return f(x);
    });
    var $elm$core$Basics$eq = _Utils_equal;
    var $elm$core$Basics$floor = _Basics_floor;
    var $elm$core$Elm$JsArray$length = _JsArray_length;
    var $elm$core$Basics$max = F2(function(x, y) {
      return _Utils_cmp(x, y) > 0 ? x : y;
    });
    var $elm$core$Basics$mul = _Basics_mul;
    var $elm$core$Array$SubTree = function(a) {
      return { $: 0, a };
    };
    var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
    var $elm$core$Array$compressNodes = F2(function(nodes, acc) {
      compressNodes:
        while (true) {
          var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
          var node = _v0.a;
          var remainingNodes = _v0.b;
          var newAcc = A2($elm$core$List$cons, $elm$core$Array$SubTree(node), acc);
          if (!remainingNodes.b) {
            return $elm$core$List$reverse(newAcc);
          } else {
            var $temp$nodes = remainingNodes, $temp$acc = newAcc;
            nodes = $temp$nodes;
            acc = $temp$acc;
            continue compressNodes;
          }
        }
    });
    var $elm$core$Tuple$first = function(_v0) {
      var x = _v0.a;
      return x;
    };
    var $elm$core$Array$treeFromBuilder = F2(function(nodeList, nodeListSize) {
      treeFromBuilder:
        while (true) {
          var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
          if (newNodeSize === 1) {
            return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
          } else {
            var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil), $temp$nodeListSize = newNodeSize;
            nodeList = $temp$nodeList;
            nodeListSize = $temp$nodeListSize;
            continue treeFromBuilder;
          }
        }
    });
    var $elm$core$Array$builderToArray = F2(function(reverseNodeList, builder) {
      if (!builder.k) {
        return A4($elm$core$Array$Array_elm_builtin, $elm$core$Elm$JsArray$length(builder.l), $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, builder.l);
      } else {
        var treeLen = builder.k * $elm$core$Array$branchFactor;
        var depth = $elm$core$Basics$floor(A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
        var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.n) : builder.n;
        var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.k);
        return A4($elm$core$Array$Array_elm_builtin, $elm$core$Elm$JsArray$length(builder.l) + treeLen, A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep), tree, builder.l);
      }
    });
    var $elm$core$Basics$idiv = _Basics_idiv;
    var $elm$core$Basics$lt = _Utils_lt;
    var $elm$core$Array$initializeHelp = F5(function(fn, fromIndex, len, nodeList, tail) {
      initializeHelp:
        while (true) {
          if (fromIndex < 0) {
            return A2($elm$core$Array$builderToArray, false, { n: nodeList, k: len / $elm$core$Array$branchFactor | 0, l: tail });
          } else {
            var leaf = $elm$core$Array$Leaf(A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
            var $temp$fn = fn, $temp$fromIndex = fromIndex - $elm$core$Array$branchFactor, $temp$len = len, $temp$nodeList = A2($elm$core$List$cons, leaf, nodeList), $temp$tail = tail;
            fn = $temp$fn;
            fromIndex = $temp$fromIndex;
            len = $temp$len;
            nodeList = $temp$nodeList;
            tail = $temp$tail;
            continue initializeHelp;
          }
        }
    });
    var $elm$core$Basics$remainderBy = _Basics_remainderBy;
    var $elm$core$Array$initialize = F2(function(len, fn) {
      if (len <= 0) {
        return $elm$core$Array$empty;
      } else {
        var tailLen = len % $elm$core$Array$branchFactor;
        var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
        var initialFromIndex = len - tailLen - $elm$core$Array$branchFactor;
        return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
      }
    });
    var $elm$core$Basics$True = 0;
    var $elm$core$Result$isOk = function(result) {
      if (!result.$) {
        return true;
      } else {
        return false;
      }
    };
    var $elm$json$Json$Decode$andThen = _Json_andThen;
    var $elm$json$Json$Decode$fail = _Json_fail;
    var $elm$parser$Parser$DeadEnd = F3(function(row, col, problem) {
      return { cl: col, U: problem, jn: row };
    });
    var $elm$parser$Parser$problemToDeadEnd = function(p) {
      return A3($elm$parser$Parser$DeadEnd, p.jn, p.cl, p.U);
    };
    var $elm$parser$Parser$Advanced$bagToList = F2(function(bag, list) {
      bagToList:
        while (true) {
          switch (bag.$) {
            case 0:
              return list;
            case 1:
              var bag1 = bag.a;
              var x = bag.b;
              var $temp$bag = bag1, $temp$list = A2($elm$core$List$cons, x, list);
              bag = $temp$bag;
              list = $temp$list;
              continue bagToList;
            default:
              var bag1 = bag.a;
              var bag2 = bag.b;
              var $temp$bag = bag1, $temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
              bag = $temp$bag;
              list = $temp$list;
              continue bagToList;
          }
        }
    });
    var $elm$parser$Parser$Advanced$run = F2(function(_v0, src) {
      var parse = _v0;
      var _v1 = parse({ cl: 1, iu: _List_Nil, h: 1, d: 0, jn: 1, g_: src });
      if (!_v1.$) {
        var value = _v1.b;
        return $elm$core$Result$Ok(value);
      } else {
        var bag = _v1.b;
        return $elm$core$Result$Err(A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
      }
    });
    var $elm$parser$Parser$run = F2(function(parser, source) {
      var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
      if (!_v0.$) {
        var a = _v0.a;
        return $elm$core$Result$Ok(a);
      } else {
        var problems = _v0.a;
        return $elm$core$Result$Err(A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
      }
    });
    var $elm$parser$Parser$Done = function(a) {
      return { $: 1, a };
    };
    var $elm$parser$Parser$Forbidden = 0;
    var $elm$project_metadata_utils$Elm$Type$Lambda = F2(function(a, b) {
      return { $: 1, a, b };
    });
    var $elm$parser$Parser$Loop = function(a) {
      return { $: 0, a };
    };
    var $elm$project_metadata_utils$Elm$Type$Record = F2(function(a, b) {
      return { $: 4, a, b };
    });
    var $elm$project_metadata_utils$Elm$Type$Type = F2(function(a, b) {
      return { $: 3, a, b };
    });
    var $elm$project_metadata_utils$Elm$Type$Var = function(a) {
      return { $: 0, a };
    };
    var $elm$parser$Parser$Advanced$Bad = F2(function(a, b) {
      return { $: 1, a, b };
    });
    var $elm$parser$Parser$Advanced$Good = F3(function(a, b, c) {
      return { $: 0, a, b, c };
    });
    var $elm$core$Basics$identity = function(x) {
      return x;
    };
    var $elm$parser$Parser$Advanced$Parser = $elm$core$Basics$identity;
    var $elm$parser$Parser$Advanced$andThen = F2(function(callback, _v0) {
      var parseA = _v0;
      return function(s0) {
        var _v1 = parseA(s0);
        if (_v1.$ === 1) {
          var p = _v1.a;
          var x = _v1.b;
          return A2($elm$parser$Parser$Advanced$Bad, p, x);
        } else {
          var p1 = _v1.a;
          var a = _v1.b;
          var s1 = _v1.c;
          var _v2 = callback(a);
          var parseB = _v2;
          var _v3 = parseB(s1);
          if (_v3.$ === 1) {
            var p2 = _v3.a;
            var x = _v3.b;
            return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
          } else {
            var p2 = _v3.a;
            var b = _v3.b;
            var s2 = _v3.c;
            return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
          }
        }
      };
    });
    var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
    var $elm$parser$Parser$ExpectingSymbol = function(a) {
      return { $: 8, a };
    };
    var $elm$parser$Parser$Advanced$Token = F2(function(a, b) {
      return { $: 0, a, b };
    });
    var $elm$parser$Parser$Advanced$AddRight = F2(function(a, b) {
      return { $: 1, a, b };
    });
    var $elm$parser$Parser$Advanced$DeadEnd = F4(function(row, col, problem, contextStack) {
      return { cl: col, iv: contextStack, U: problem, jn: row };
    });
    var $elm$parser$Parser$Advanced$Empty = { $: 0 };
    var $elm$parser$Parser$Advanced$fromState = F2(function(s, x) {
      return A2($elm$parser$Parser$Advanced$AddRight, $elm$parser$Parser$Advanced$Empty, A4($elm$parser$Parser$Advanced$DeadEnd, s.jn, s.cl, x, s.iu));
    });
    var $elm$core$String$isEmpty = function(string) {
      return string === "";
    };
    var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
    var $elm$core$Basics$negate = function(n) {
      return -n;
    };
    var $elm$core$Basics$not = _Basics_not;
    var $elm$parser$Parser$Advanced$token = function(_v0) {
      var str = _v0.a;
      var expecting = _v0.b;
      var progress = !$elm$core$String$isEmpty(str);
      return function(s) {
        var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.d, s.jn, s.cl, s.g_);
        var newOffset = _v1.a;
        var newRow = _v1.b;
        var newCol = _v1.c;
        return _Utils_eq(newOffset, -1) ? A2($elm$parser$Parser$Advanced$Bad, false, A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3($elm$parser$Parser$Advanced$Good, progress, 0, { cl: newCol, iu: s.iu, h: s.h, d: newOffset, jn: newRow, g_: s.g_ });
      };
    };
    var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
    var $elm$parser$Parser$symbol = function(str) {
      return $elm$parser$Parser$Advanced$symbol(A2($elm$parser$Parser$Advanced$Token, str, $elm$parser$Parser$ExpectingSymbol(str)));
    };
    var $elm$project_metadata_utils$Elm$Type$arrow = $elm$parser$Parser$symbol("->");
    var $elm$parser$Parser$Advanced$backtrackable = function(_v0) {
      var parse = _v0;
      return function(s0) {
        var _v1 = parse(s0);
        if (_v1.$ === 1) {
          var x = _v1.b;
          return A2($elm$parser$Parser$Advanced$Bad, false, x);
        } else {
          var a = _v1.b;
          var s1 = _v1.c;
          return A3($elm$parser$Parser$Advanced$Good, false, a, s1);
        }
      };
    };
    var $elm$parser$Parser$backtrackable = $elm$parser$Parser$Advanced$backtrackable;
    var $elm$project_metadata_utils$Elm$Type$comma = $elm$parser$Parser$symbol(",");
    var $elm$core$Basics$always = F2(function(a, _v0) {
      return a;
    });
    var $elm$parser$Parser$Advanced$map2 = F3(function(func, _v0, _v1) {
      var parseA = _v0;
      var parseB = _v1;
      return function(s0) {
        var _v2 = parseA(s0);
        if (_v2.$ === 1) {
          var p = _v2.a;
          var x = _v2.b;
          return A2($elm$parser$Parser$Advanced$Bad, p, x);
        } else {
          var p1 = _v2.a;
          var a = _v2.b;
          var s1 = _v2.c;
          var _v3 = parseB(s1);
          if (_v3.$ === 1) {
            var p2 = _v3.a;
            var x = _v3.b;
            return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
          } else {
            var p2 = _v3.a;
            var b = _v3.b;
            var s2 = _v3.c;
            return A3($elm$parser$Parser$Advanced$Good, p1 || p2, A2(func, a, b), s2);
          }
        }
      };
    });
    var $elm$parser$Parser$Advanced$ignorer = F2(function(keepParser, ignoreParser) {
      return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
    });
    var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
    var $elm$parser$Parser$Advanced$keeper = F2(function(parseFunc, parseArg) {
      return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
    });
    var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
    var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
    var $elm$core$Dict$RBEmpty_elm_builtin = { $: -2 };
    var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
    var $elm$core$Set$empty = $elm$core$Dict$empty;
    var $elm$project_metadata_utils$Elm$Type$isInnerVarChar = function(_char) {
      return $elm$core$Char$isAlphaNum(_char) || _char === "_";
    };
    var $elm$parser$Parser$ExpectingVariable = { $: 7 };
    var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
    var $elm$core$Basics$compare = _Utils_compare;
    var $elm$core$Dict$get = F2(function(targetKey, dict) {
      get:
        while (true) {
          if (dict.$ === -2) {
            return $elm$core$Maybe$Nothing;
          } else {
            var key = dict.b;
            var value = dict.c;
            var left = dict.d;
            var right = dict.e;
            var _v1 = A2($elm$core$Basics$compare, targetKey, key);
            switch (_v1) {
              case 0:
                var $temp$targetKey = targetKey, $temp$dict = left;
                targetKey = $temp$targetKey;
                dict = $temp$dict;
                continue get;
              case 1:
                return $elm$core$Maybe$Just(value);
              default:
                var $temp$targetKey = targetKey, $temp$dict = right;
                targetKey = $temp$targetKey;
                dict = $temp$dict;
                continue get;
            }
          }
        }
    });
    var $elm$core$Dict$member = F2(function(key, dict) {
      var _v0 = A2($elm$core$Dict$get, key, dict);
      if (!_v0.$) {
        return true;
      } else {
        return false;
      }
    });
    var $elm$core$Set$member = F2(function(key, _v0) {
      var dict = _v0;
      return A2($elm$core$Dict$member, key, dict);
    });
    var $elm$core$String$slice = _String_slice;
    var $elm$parser$Parser$Advanced$varHelp = F7(function(isGood, offset, row, col, src, indent, context) {
      varHelp:
        while (true) {
          var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, src);
          if (_Utils_eq(newOffset, -1)) {
            return { cl: col, iu: context, h: indent, d: offset, jn: row, g_: src };
          } else {
            if (_Utils_eq(newOffset, -2)) {
              var $temp$isGood = isGood, $temp$offset = offset + 1, $temp$row = row + 1, $temp$col = 1, $temp$src = src, $temp$indent = indent, $temp$context = context;
              isGood = $temp$isGood;
              offset = $temp$offset;
              row = $temp$row;
              col = $temp$col;
              src = $temp$src;
              indent = $temp$indent;
              context = $temp$context;
              continue varHelp;
            } else {
              var $temp$isGood = isGood, $temp$offset = newOffset, $temp$row = row, $temp$col = col + 1, $temp$src = src, $temp$indent = indent, $temp$context = context;
              isGood = $temp$isGood;
              offset = $temp$offset;
              row = $temp$row;
              col = $temp$col;
              src = $temp$src;
              indent = $temp$indent;
              context = $temp$context;
              continue varHelp;
            }
          }
        }
    });
    var $elm$parser$Parser$Advanced$variable = function(i) {
      return function(s) {
        var firstOffset = A3($elm$parser$Parser$Advanced$isSubChar, i.z, s.d, s.g_);
        if (_Utils_eq(firstOffset, -1)) {
          return A2($elm$parser$Parser$Advanced$Bad, false, A2($elm$parser$Parser$Advanced$fromState, s, i.dh));
        } else {
          var s1 = _Utils_eq(firstOffset, -2) ? A7($elm$parser$Parser$Advanced$varHelp, i.iS, s.d + 1, s.jn + 1, 1, s.g_, s.h, s.iu) : A7($elm$parser$Parser$Advanced$varHelp, i.iS, firstOffset, s.jn, s.cl + 1, s.g_, s.h, s.iu);
          var name = A3($elm$core$String$slice, s.d, s1.d, s.g_);
          return A2($elm$core$Set$member, name, i.jk) ? A2($elm$parser$Parser$Advanced$Bad, false, A2($elm$parser$Parser$Advanced$fromState, s, i.dh)) : A3($elm$parser$Parser$Advanced$Good, true, name, s1);
        }
      };
    };
    var $elm$parser$Parser$variable = function(i) {
      return $elm$parser$Parser$Advanced$variable({ dh: $elm$parser$Parser$ExpectingVariable, iS: i.iS, jk: i.jk, z: i.z });
    };
    var $elm$project_metadata_utils$Elm$Type$var = function(isFirst) {
      return $elm$parser$Parser$variable({ iS: $elm$project_metadata_utils$Elm$Type$isInnerVarChar, jk: $elm$core$Set$empty, z: isFirst });
    };
    var $elm$project_metadata_utils$Elm$Type$lowVar = $elm$project_metadata_utils$Elm$Type$var($elm$core$Char$isLower);
    var $elm$parser$Parser$Advanced$Append = F2(function(a, b) {
      return { $: 2, a, b };
    });
    var $elm$parser$Parser$Advanced$oneOfHelp = F3(function(s0, bag, parsers) {
      oneOfHelp:
        while (true) {
          if (!parsers.b) {
            return A2($elm$parser$Parser$Advanced$Bad, false, bag);
          } else {
            var parse = parsers.a;
            var remainingParsers = parsers.b;
            var _v1 = parse(s0);
            if (!_v1.$) {
              var step = _v1;
              return step;
            } else {
              var step = _v1;
              var p = step.a;
              var x = step.b;
              if (p) {
                return step;
              } else {
                var $temp$s0 = s0, $temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x), $temp$parsers = remainingParsers;
                s0 = $temp$s0;
                bag = $temp$bag;
                parsers = $temp$parsers;
                continue oneOfHelp;
              }
            }
          }
        }
    });
    var $elm$parser$Parser$Advanced$oneOf = function(parsers) {
      return function(s) {
        return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
      };
    };
    var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
    var $elm$parser$Parser$Advanced$chompWhileHelp = F5(function(isGood, offset, row, col, s0) {
      chompWhileHelp:
        while (true) {
          var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.g_);
          if (_Utils_eq(newOffset, -1)) {
            return A3($elm$parser$Parser$Advanced$Good, _Utils_cmp(s0.d, offset) < 0, 0, { cl: col, iu: s0.iu, h: s0.h, d: offset, jn: row, g_: s0.g_ });
          } else {
            if (_Utils_eq(newOffset, -2)) {
              var $temp$isGood = isGood, $temp$offset = offset + 1, $temp$row = row + 1, $temp$col = 1, $temp$s0 = s0;
              isGood = $temp$isGood;
              offset = $temp$offset;
              row = $temp$row;
              col = $temp$col;
              s0 = $temp$s0;
              continue chompWhileHelp;
            } else {
              var $temp$isGood = isGood, $temp$offset = newOffset, $temp$row = row, $temp$col = col + 1, $temp$s0 = s0;
              isGood = $temp$isGood;
              offset = $temp$offset;
              row = $temp$row;
              col = $temp$col;
              s0 = $temp$s0;
              continue chompWhileHelp;
            }
          }
        }
    });
    var $elm$parser$Parser$Advanced$chompWhile = function(isGood) {
      return function(s) {
        return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.d, s.jn, s.cl, s);
      };
    };
    var $elm$parser$Parser$chompWhile = $elm$parser$Parser$Advanced$chompWhile;
    var $elm$project_metadata_utils$Elm$Type$spaces = $elm$parser$Parser$chompWhile(function(_char) {
      return _char === " ";
    });
    var $elm$parser$Parser$Advanced$succeed = function(a) {
      return function(s) {
        return A3($elm$parser$Parser$Advanced$Good, false, a, s);
      };
    };
    var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
    var $elm$project_metadata_utils$Elm$Type$extension = $elm$parser$Parser$oneOf(_List_fromArray([
      A2($elm$parser$Parser$keeper, $elm$parser$Parser$succeed($elm$core$Maybe$Just), A2($elm$parser$Parser$ignorer, A2($elm$parser$Parser$ignorer, A2($elm$parser$Parser$ignorer, $elm$parser$Parser$backtrackable($elm$project_metadata_utils$Elm$Type$lowVar), $elm$parser$Parser$backtrackable($elm$project_metadata_utils$Elm$Type$spaces)), $elm$parser$Parser$symbol("|")), $elm$project_metadata_utils$Elm$Type$spaces)),
      $elm$parser$Parser$succeed($elm$core$Maybe$Nothing)
    ]));
    var $elm$parser$Parser$Advanced$lazy = function(thunk) {
      return function(s) {
        var _v0 = thunk(0);
        var parse = _v0;
        return parse(s);
      };
    };
    var $elm$parser$Parser$lazy = $elm$parser$Parser$Advanced$lazy;
    var $elm$parser$Parser$Advanced$loopHelp = F4(function(p, state, callback, s0) {
      loopHelp:
        while (true) {
          var _v0 = callback(state);
          var parse = _v0;
          var _v1 = parse(s0);
          if (!_v1.$) {
            var p1 = _v1.a;
            var step = _v1.b;
            var s1 = _v1.c;
            if (!step.$) {
              var newState = step.a;
              var $temp$p = p || p1, $temp$state = newState, $temp$callback = callback, $temp$s0 = s1;
              p = $temp$p;
              state = $temp$state;
              callback = $temp$callback;
              s0 = $temp$s0;
              continue loopHelp;
            } else {
              var result = step.a;
              return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
            }
          } else {
            var p1 = _v1.a;
            var x = _v1.b;
            return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
          }
        }
    });
    var $elm$parser$Parser$Advanced$loop = F2(function(state, callback) {
      return function(s) {
        return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
      };
    });
    var $elm$parser$Parser$Advanced$map = F2(function(func, _v0) {
      var parse = _v0;
      return function(s0) {
        var _v1 = parse(s0);
        if (!_v1.$) {
          var p = _v1.a;
          var a = _v1.b;
          var s1 = _v1.c;
          return A3($elm$parser$Parser$Advanced$Good, p, func(a), s1);
        } else {
          var p = _v1.a;
          var x = _v1.b;
          return A2($elm$parser$Parser$Advanced$Bad, p, x);
        }
      };
    });
    var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
    var $elm$parser$Parser$Advanced$Done = function(a) {
      return { $: 1, a };
    };
    var $elm$parser$Parser$Advanced$Loop = function(a) {
      return { $: 0, a };
    };
    var $elm$parser$Parser$toAdvancedStep = function(step) {
      if (!step.$) {
        var s = step.a;
        return $elm$parser$Parser$Advanced$Loop(s);
      } else {
        var a = step.a;
        return $elm$parser$Parser$Advanced$Done(a);
      }
    };
    var $elm$parser$Parser$loop = F2(function(state, callback) {
      return A2($elm$parser$Parser$Advanced$loop, state, function(s) {
        return A2($elm$parser$Parser$map, $elm$parser$Parser$toAdvancedStep, callback(s));
      });
    });
    var $elm$core$Tuple$pair = F2(function(a, b) {
      return _Utils_Tuple2(a, b);
    });
    var $elm$project_metadata_utils$Elm$Type$capVar = $elm$project_metadata_utils$Elm$Type$var($elm$core$Char$isUpper);
    var $elm$parser$Parser$Advanced$mapChompedString = F2(function(func, _v0) {
      var parse = _v0;
      return function(s0) {
        var _v1 = parse(s0);
        if (_v1.$ === 1) {
          var p = _v1.a;
          var x = _v1.b;
          return A2($elm$parser$Parser$Advanced$Bad, p, x);
        } else {
          var p = _v1.a;
          var a = _v1.b;
          var s1 = _v1.c;
          return A3($elm$parser$Parser$Advanced$Good, p, A2(func, A3($elm$core$String$slice, s0.d, s1.d, s0.g_), a), s1);
        }
      };
    });
    var $elm$parser$Parser$Advanced$getChompedString = function(parser) {
      return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
    };
    var $elm$parser$Parser$getChompedString = $elm$parser$Parser$Advanced$getChompedString;
    var $elm$project_metadata_utils$Elm$Type$qualifiedCapVarHelp = function(_v0) {
      return $elm$parser$Parser$oneOf(_List_fromArray([
        A2($elm$parser$Parser$ignorer, A2($elm$parser$Parser$ignorer, $elm$parser$Parser$succeed($elm$parser$Parser$Loop(0)), $elm$parser$Parser$symbol(".")), $elm$project_metadata_utils$Elm$Type$capVar),
        $elm$parser$Parser$succeed($elm$parser$Parser$Done(0))
      ]));
    };
    var $elm$project_metadata_utils$Elm$Type$qualifiedCapVar = $elm$parser$Parser$getChompedString(A2($elm$parser$Parser$ignorer, $elm$project_metadata_utils$Elm$Type$capVar, A2($elm$parser$Parser$loop, 0, $elm$project_metadata_utils$Elm$Type$qualifiedCapVarHelp)));
    var $elm$parser$Parser$Advanced$revAlways = F2(function(_v0, b) {
      return b;
    });
    var $elm$parser$Parser$Advanced$skip = F2(function(iParser, kParser) {
      return A3($elm$parser$Parser$Advanced$map2, $elm$parser$Parser$Advanced$revAlways, iParser, kParser);
    });
    var $elm$parser$Parser$Advanced$sequenceEndForbidden = F5(function(ender, ws, parseItem, sep, revItems) {
      var chompRest = function(item) {
        return A5($elm$parser$Parser$Advanced$sequenceEndForbidden, ender, ws, parseItem, sep, A2($elm$core$List$cons, item, revItems));
      };
      return A2($elm$parser$Parser$Advanced$skip, ws, $elm$parser$Parser$Advanced$oneOf(_List_fromArray([
        A2($elm$parser$Parser$Advanced$skip, sep, A2($elm$parser$Parser$Advanced$skip, ws, A2($elm$parser$Parser$Advanced$map, function(item) {
          return $elm$parser$Parser$Advanced$Loop(A2($elm$core$List$cons, item, revItems));
        }, parseItem))),
        A2($elm$parser$Parser$Advanced$map, function(_v0) {
          return $elm$parser$Parser$Advanced$Done($elm$core$List$reverse(revItems));
        }, ender)
      ])));
    });
    var $elm$parser$Parser$Advanced$sequenceEndMandatory = F4(function(ws, parseItem, sep, revItems) {
      return $elm$parser$Parser$Advanced$oneOf(_List_fromArray([
        A2($elm$parser$Parser$Advanced$map, function(item) {
          return $elm$parser$Parser$Advanced$Loop(A2($elm$core$List$cons, item, revItems));
        }, A2($elm$parser$Parser$Advanced$ignorer, parseItem, A2($elm$parser$Parser$Advanced$ignorer, ws, A2($elm$parser$Parser$Advanced$ignorer, sep, ws)))),
        A2($elm$parser$Parser$Advanced$map, function(_v0) {
          return $elm$parser$Parser$Advanced$Done($elm$core$List$reverse(revItems));
        }, $elm$parser$Parser$Advanced$succeed(0))
      ]));
    });
    var $elm$parser$Parser$Advanced$sequenceEndOptional = F5(function(ender, ws, parseItem, sep, revItems) {
      var parseEnd = A2($elm$parser$Parser$Advanced$map, function(_v0) {
        return $elm$parser$Parser$Advanced$Done($elm$core$List$reverse(revItems));
      }, ender);
      return A2($elm$parser$Parser$Advanced$skip, ws, $elm$parser$Parser$Advanced$oneOf(_List_fromArray([
        A2($elm$parser$Parser$Advanced$skip, sep, A2($elm$parser$Parser$Advanced$skip, ws, $elm$parser$Parser$Advanced$oneOf(_List_fromArray([
          A2($elm$parser$Parser$Advanced$map, function(item) {
            return $elm$parser$Parser$Advanced$Loop(A2($elm$core$List$cons, item, revItems));
          }, parseItem),
          parseEnd
        ])))),
        parseEnd
      ])));
    });
    var $elm$parser$Parser$Advanced$sequenceEnd = F5(function(ender, ws, parseItem, sep, trailing) {
      var chompRest = function(item) {
        switch (trailing) {
          case 0:
            return A2($elm$parser$Parser$Advanced$loop, _List_fromArray([item]), A4($elm$parser$Parser$Advanced$sequenceEndForbidden, ender, ws, parseItem, sep));
          case 1:
            return A2($elm$parser$Parser$Advanced$loop, _List_fromArray([item]), A4($elm$parser$Parser$Advanced$sequenceEndOptional, ender, ws, parseItem, sep));
          default:
            return A2($elm$parser$Parser$Advanced$ignorer, A2($elm$parser$Parser$Advanced$skip, ws, A2($elm$parser$Parser$Advanced$skip, sep, A2($elm$parser$Parser$Advanced$skip, ws, A2($elm$parser$Parser$Advanced$loop, _List_fromArray([item]), A3($elm$parser$Parser$Advanced$sequenceEndMandatory, ws, parseItem, sep))))), ender);
        }
      };
      return $elm$parser$Parser$Advanced$oneOf(_List_fromArray([
        A2($elm$parser$Parser$Advanced$andThen, chompRest, parseItem),
        A2($elm$parser$Parser$Advanced$map, function(_v0) {
          return _List_Nil;
        }, ender)
      ]));
    });
    var $elm$parser$Parser$Advanced$sequence = function(i) {
      return A2($elm$parser$Parser$Advanced$skip, $elm$parser$Parser$Advanced$token(i.z), A2($elm$parser$Parser$Advanced$skip, i.gW, A5($elm$parser$Parser$Advanced$sequenceEnd, $elm$parser$Parser$Advanced$token(i.aT), i.gW, i.iW, $elm$parser$Parser$Advanced$token(i.jp), i.bq)));
    };
    var $elm$parser$Parser$Advanced$Forbidden = 0;
    var $elm$parser$Parser$Advanced$Mandatory = 2;
    var $elm$parser$Parser$Advanced$Optional = 1;
    var $elm$parser$Parser$toAdvancedTrailing = function(trailing) {
      switch (trailing) {
        case 0:
          return 0;
        case 1:
          return 1;
        default:
          return 2;
      }
    };
    var $elm$parser$Parser$Expecting = function(a) {
      return { $: 0, a };
    };
    var $elm$parser$Parser$toToken = function(str) {
      return A2($elm$parser$Parser$Advanced$Token, str, $elm$parser$Parser$Expecting(str));
    };
    var $elm$parser$Parser$sequence = function(i) {
      return $elm$parser$Parser$Advanced$sequence({
        aT: $elm$parser$Parser$toToken(i.aT),
        iW: i.iW,
        jp: $elm$parser$Parser$toToken(i.jp),
        gW: i.gW,
        z: $elm$parser$Parser$toToken(i.z),
        bq: $elm$parser$Parser$toAdvancedTrailing(i.bq)
      });
    };
    var $elm$project_metadata_utils$Elm$Type$Tuple = function(a) {
      return { $: 2, a };
    };
    var $elm$project_metadata_utils$Elm$Type$tuplize = function(args) {
      if (args.b && !args.b.b) {
        var arg = args.a;
        return arg;
      } else {
        return $elm$project_metadata_utils$Elm$Type$Tuple(args);
      }
    };
    var $elm$project_metadata_utils$Elm$Type$chompArgs = function(revArgs) {
      return $elm$parser$Parser$oneOf(_List_fromArray([
        A2($elm$parser$Parser$map, function(arg) {
          return $elm$parser$Parser$Loop(A2($elm$core$List$cons, arg, revArgs));
        }, A2($elm$parser$Parser$keeper, A2($elm$parser$Parser$ignorer, $elm$parser$Parser$succeed($elm$core$Basics$identity), $elm$parser$Parser$backtrackable($elm$project_metadata_utils$Elm$Type$spaces)), $elm$project_metadata_utils$Elm$Type$cyclic$term())),
        A2($elm$parser$Parser$map, function(_v2) {
          return $elm$parser$Parser$Done($elm$core$List$reverse(revArgs));
        }, $elm$parser$Parser$succeed(0))
      ]));
    };
    var $elm$project_metadata_utils$Elm$Type$recordEndHelp = function(revFields) {
      return $elm$parser$Parser$oneOf(_List_fromArray([
        A2($elm$parser$Parser$keeper, A2($elm$parser$Parser$ignorer, A2($elm$parser$Parser$ignorer, $elm$parser$Parser$succeed(function(f) {
          return $elm$parser$Parser$Loop(A2($elm$core$List$cons, f, revFields));
        }), $elm$project_metadata_utils$Elm$Type$comma), $elm$project_metadata_utils$Elm$Type$spaces), A2($elm$parser$Parser$ignorer, $elm$project_metadata_utils$Elm$Type$cyclic$field(), $elm$project_metadata_utils$Elm$Type$spaces)),
        A2($elm$parser$Parser$keeper, $elm$parser$Parser$succeed(function(_v1) {
          return $elm$parser$Parser$Done($elm$core$List$reverse(revFields));
        }), $elm$parser$Parser$symbol("}"))
      ]));
    };
    var $elm$project_metadata_utils$Elm$Type$tipeHelp = function(t) {
      return $elm$parser$Parser$oneOf(_List_fromArray([
        A2($elm$parser$Parser$map, $elm$project_metadata_utils$Elm$Type$Lambda(t), $elm$project_metadata_utils$Elm$Type$cyclic$arrowAndType()),
        $elm$parser$Parser$succeed(t)
      ]));
    };
    function $elm$project_metadata_utils$Elm$Type$cyclic$arrowAndType() {
      return A2($elm$parser$Parser$keeper, A2($elm$parser$Parser$ignorer, A2($elm$parser$Parser$ignorer, A2($elm$parser$Parser$ignorer, $elm$parser$Parser$succeed($elm$core$Basics$identity), $elm$parser$Parser$backtrackable($elm$project_metadata_utils$Elm$Type$spaces)), $elm$project_metadata_utils$Elm$Type$arrow), $elm$project_metadata_utils$Elm$Type$spaces), $elm$project_metadata_utils$Elm$Type$cyclic$tipe());
    }
    function $elm$project_metadata_utils$Elm$Type$cyclic$tipeTerm() {
      return $elm$parser$Parser$oneOf(_List_fromArray([
        A2($elm$parser$Parser$map, $elm$project_metadata_utils$Elm$Type$Var, $elm$project_metadata_utils$Elm$Type$lowVar),
        A2($elm$parser$Parser$keeper, A2($elm$parser$Parser$keeper, $elm$parser$Parser$succeed($elm$project_metadata_utils$Elm$Type$Type), $elm$project_metadata_utils$Elm$Type$qualifiedCapVar), A2($elm$parser$Parser$loop, _List_Nil, $elm$project_metadata_utils$Elm$Type$chompArgs)),
        $elm$project_metadata_utils$Elm$Type$cyclic$record(),
        $elm$project_metadata_utils$Elm$Type$cyclic$tuple()
      ]));
    }
    function $elm$project_metadata_utils$Elm$Type$cyclic$term() {
      return $elm$parser$Parser$oneOf(_List_fromArray([
        A2($elm$parser$Parser$map, $elm$project_metadata_utils$Elm$Type$Var, $elm$project_metadata_utils$Elm$Type$lowVar),
        A2($elm$parser$Parser$map, function(name) {
          return A2($elm$project_metadata_utils$Elm$Type$Type, name, _List_Nil);
        }, $elm$project_metadata_utils$Elm$Type$qualifiedCapVar),
        $elm$project_metadata_utils$Elm$Type$cyclic$record(),
        $elm$project_metadata_utils$Elm$Type$cyclic$tuple()
      ]));
    }
    function $elm$project_metadata_utils$Elm$Type$cyclic$record() {
      return A2($elm$parser$Parser$keeper, A2($elm$parser$Parser$keeper, A2($elm$parser$Parser$ignorer, A2($elm$parser$Parser$ignorer, $elm$parser$Parser$succeed(F2(function(ext, fs) {
        return A2($elm$project_metadata_utils$Elm$Type$Record, fs, ext);
      })), $elm$parser$Parser$symbol("{")), $elm$project_metadata_utils$Elm$Type$spaces), $elm$project_metadata_utils$Elm$Type$extension), $elm$project_metadata_utils$Elm$Type$cyclic$recordEnd());
    }
    function $elm$project_metadata_utils$Elm$Type$cyclic$recordEnd() {
      return $elm$parser$Parser$oneOf(_List_fromArray([
        A2($elm$parser$Parser$andThen, function(f) {
          return A2($elm$parser$Parser$loop, _List_fromArray([f]), $elm$project_metadata_utils$Elm$Type$recordEndHelp);
        }, A2($elm$parser$Parser$ignorer, $elm$project_metadata_utils$Elm$Type$cyclic$field(), $elm$project_metadata_utils$Elm$Type$spaces)),
        A2($elm$parser$Parser$ignorer, $elm$parser$Parser$succeed(_List_Nil), $elm$parser$Parser$symbol("}"))
      ]));
    }
    function $elm$project_metadata_utils$Elm$Type$cyclic$field() {
      return A2($elm$parser$Parser$keeper, A2($elm$parser$Parser$keeper, $elm$parser$Parser$succeed($elm$core$Tuple$pair), A2($elm$parser$Parser$ignorer, A2($elm$parser$Parser$ignorer, A2($elm$parser$Parser$ignorer, $elm$project_metadata_utils$Elm$Type$lowVar, $elm$project_metadata_utils$Elm$Type$spaces), $elm$parser$Parser$symbol(":")), $elm$project_metadata_utils$Elm$Type$spaces)), $elm$project_metadata_utils$Elm$Type$cyclic$tipe());
    }
    function $elm$project_metadata_utils$Elm$Type$cyclic$tuple() {
      return A2($elm$parser$Parser$map, $elm$project_metadata_utils$Elm$Type$tuplize, $elm$parser$Parser$sequence({
        aT: ")",
        iW: $elm$project_metadata_utils$Elm$Type$cyclic$tipe(),
        jp: ",",
        gW: $elm$project_metadata_utils$Elm$Type$spaces,
        z: "(",
        bq: 0
      }));
    }
    function $elm$project_metadata_utils$Elm$Type$cyclic$tipe() {
      return $elm$parser$Parser$lazy(function(_v0) {
        return A2($elm$parser$Parser$andThen, $elm$project_metadata_utils$Elm$Type$tipeHelp, $elm$project_metadata_utils$Elm$Type$cyclic$tipeTerm());
      });
    }
    var $elm$project_metadata_utils$Elm$Type$arrowAndType = $elm$project_metadata_utils$Elm$Type$cyclic$arrowAndType();
    $elm$project_metadata_utils$Elm$Type$cyclic$arrowAndType = function() {
      return $elm$project_metadata_utils$Elm$Type$arrowAndType;
    };
    var $elm$project_metadata_utils$Elm$Type$tipeTerm = $elm$project_metadata_utils$Elm$Type$cyclic$tipeTerm();
    $elm$project_metadata_utils$Elm$Type$cyclic$tipeTerm = function() {
      return $elm$project_metadata_utils$Elm$Type$tipeTerm;
    };
    var $elm$project_metadata_utils$Elm$Type$term = $elm$project_metadata_utils$Elm$Type$cyclic$term();
    $elm$project_metadata_utils$Elm$Type$cyclic$term = function() {
      return $elm$project_metadata_utils$Elm$Type$term;
    };
    var $elm$project_metadata_utils$Elm$Type$record = $elm$project_metadata_utils$Elm$Type$cyclic$record();
    $elm$project_metadata_utils$Elm$Type$cyclic$record = function() {
      return $elm$project_metadata_utils$Elm$Type$record;
    };
    var $elm$project_metadata_utils$Elm$Type$recordEnd = $elm$project_metadata_utils$Elm$Type$cyclic$recordEnd();
    $elm$project_metadata_utils$Elm$Type$cyclic$recordEnd = function() {
      return $elm$project_metadata_utils$Elm$Type$recordEnd;
    };
    var $elm$project_metadata_utils$Elm$Type$field = $elm$project_metadata_utils$Elm$Type$cyclic$field();
    $elm$project_metadata_utils$Elm$Type$cyclic$field = function() {
      return $elm$project_metadata_utils$Elm$Type$field;
    };
    var $elm$project_metadata_utils$Elm$Type$tuple = $elm$project_metadata_utils$Elm$Type$cyclic$tuple();
    $elm$project_metadata_utils$Elm$Type$cyclic$tuple = function() {
      return $elm$project_metadata_utils$Elm$Type$tuple;
    };
    var $elm$project_metadata_utils$Elm$Type$tipe = $elm$project_metadata_utils$Elm$Type$cyclic$tipe();
    $elm$project_metadata_utils$Elm$Type$cyclic$tipe = function() {
      return $elm$project_metadata_utils$Elm$Type$tipe;
    };
    var $elm$project_metadata_utils$Elm$Type$parse = function(source) {
      return A2($elm$parser$Parser$run, $elm$project_metadata_utils$Elm$Type$tipe, source);
    };
    var $elm$json$Json$Decode$succeed = _Json_succeed;
    var $elm$project_metadata_utils$Elm$Type$decoderHelp = function(string) {
      var _v0 = $elm$project_metadata_utils$Elm$Type$parse(string);
      if (_v0.$ === 1) {
        var error = _v0.a;
        return $elm$json$Json$Decode$fail("TODO");
      } else {
        var actualType = _v0.a;
        return $elm$json$Json$Decode$succeed(actualType);
      }
    };
    var $elm$json$Json$Decode$string = _Json_decodeString;
    var $elm$project_metadata_utils$Elm$Type$decoder = A2($elm$json$Json$Decode$andThen, $elm$project_metadata_utils$Elm$Type$decoderHelp, $elm$json$Json$Decode$string);
    var $elm$json$Json$Decode$field = _Json_decodeField;
    var $elm$json$Json$Decode$list = _Json_decodeList;
    var $elm$json$Json$Decode$map4 = _Json_map4;
    var $elm$project_metadata_utils$Elm$Docs$aliasDecoder = A5($elm$json$Json$Decode$map4, $elm$project_metadata_utils$Elm$Docs$Alias, A2($elm$json$Json$Decode$field, "name", $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$field, "comment", $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$field, "args", $elm$json$Json$Decode$list($elm$json$Json$Decode$string)), A2($elm$json$Json$Decode$field, "type", $elm$project_metadata_utils$Elm$Type$decoder));
    var $elm$project_metadata_utils$Elm$Docs$Binop = F5(function(name, comment, tipe, associativity, precedence) {
      return { id: associativity, cp: comment, ag: name, je: precedence, ak: tipe };
    });
    var $elm$project_metadata_utils$Elm$Docs$Left = 0;
    var $elm$project_metadata_utils$Elm$Docs$None = 1;
    var $elm$project_metadata_utils$Elm$Docs$Right = 2;
    var $elm$project_metadata_utils$Elm$Docs$toAssoc = function(str) {
      switch (str) {
        case "left":
          return $elm$json$Json$Decode$succeed(0);
        case "non":
          return $elm$json$Json$Decode$succeed(1);
        case "right":
          return $elm$json$Json$Decode$succeed(2);
        default:
          return $elm$json$Json$Decode$fail("expecting one of the following values: left, non, right");
      }
    };
    var $elm$project_metadata_utils$Elm$Docs$assocDecoder = A2($elm$json$Json$Decode$andThen, $elm$project_metadata_utils$Elm$Docs$toAssoc, $elm$json$Json$Decode$string);
    var $elm$json$Json$Decode$int = _Json_decodeInt;
    var $elm$json$Json$Decode$map5 = _Json_map5;
    var $elm$project_metadata_utils$Elm$Docs$binopDecoder = A6($elm$json$Json$Decode$map5, $elm$project_metadata_utils$Elm$Docs$Binop, A2($elm$json$Json$Decode$field, "name", $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$field, "comment", $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$field, "type", $elm$project_metadata_utils$Elm$Type$decoder), A2($elm$json$Json$Decode$field, "associativity", $elm$project_metadata_utils$Elm$Docs$assocDecoder), A2($elm$json$Json$Decode$field, "precedence", $elm$json$Json$Decode$int));
    var $elm$json$Json$Decode$map6 = _Json_map6;
    var $elm$project_metadata_utils$Elm$Docs$Union = F4(function(name, comment, args, tags) {
      return { G: args, cp: comment, ag: name, aj: tags };
    });
    var $elm$json$Json$Decode$index = _Json_decodeIndex;
    var $elm$json$Json$Decode$map2 = _Json_map2;
    var $elm$project_metadata_utils$Elm$Docs$tagDecoder = A3($elm$json$Json$Decode$map2, F2(function(a, b) {
      return _Utils_Tuple2(a, b);
    }), A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Type$decoder)));
    var $elm$project_metadata_utils$Elm$Docs$unionDecoder = A5($elm$json$Json$Decode$map4, $elm$project_metadata_utils$Elm$Docs$Union, A2($elm$json$Json$Decode$field, "name", $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$field, "comment", $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$field, "args", $elm$json$Json$Decode$list($elm$json$Json$Decode$string)), A2($elm$json$Json$Decode$field, "cases", $elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$tagDecoder)));
    var $elm$project_metadata_utils$Elm$Docs$Value = F3(function(name, comment, tipe) {
      return { cp: comment, ag: name, ak: tipe };
    });
    var $elm$json$Json$Decode$map3 = _Json_map3;
    var $elm$project_metadata_utils$Elm$Docs$valueDecoder = A4($elm$json$Json$Decode$map3, $elm$project_metadata_utils$Elm$Docs$Value, A2($elm$json$Json$Decode$field, "name", $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$field, "comment", $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$field, "type", $elm$project_metadata_utils$Elm$Type$decoder));
    var $elm$project_metadata_utils$Elm$Docs$decoder = A7($elm$json$Json$Decode$map6, $elm$project_metadata_utils$Elm$Docs$Module, A2($elm$json$Json$Decode$field, "name", $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$field, "comment", $elm$json$Json$Decode$string), A2($elm$json$Json$Decode$field, "unions", $elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$unionDecoder)), A2($elm$json$Json$Decode$field, "aliases", $elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$aliasDecoder)), A2($elm$json$Json$Decode$field, "values", $elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$valueDecoder)), A2($elm$json$Json$Decode$field, "binops", $elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$binopDecoder)));
    var $author$project$Options$decoder = A3($elm$json$Json$Decode$map2, $author$project$Options$Options, A2($elm$json$Json$Decode$field, "project", $elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$decoder)), A2($elm$json$Json$Decode$field, "viewers", $elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$decoder)));
    var $elm$json$Json$Decode$decodeValue = _Json_run;
    var $elm$json$Json$Encode$list = F2(function(func, entries) {
      return _Json_wrap(A3($elm$core$List$foldl, _Json_addEntry(func), _Json_emptyArray(0), entries));
    });
    var $elm$json$Json$Encode$object = function(pairs) {
      return _Json_wrap(A3($elm$core$List$foldl, F2(function(_v0, obj) {
        var k = _v0.a;
        var v = _v0.b;
        return A3(_Json_addField, k, v, obj);
      }), _Json_emptyObject(0), pairs));
    };
    var $elm$json$Json$Encode$string = _Json_wrap;
    var $author$project$Gen$CodeGen$Generate$onFailureSend = _Platform_outgoingPort("onFailureSend", $elm$json$Json$Encode$list(function($) {
      return $elm$json$Json$Encode$object(_List_fromArray([
        _Utils_Tuple2("description", $elm$json$Json$Encode$string($.cP)),
        _Utils_Tuple2("title", $elm$json$Json$Encode$string($.jz))
      ]));
    }));
    var $author$project$Gen$CodeGen$Generate$error = function(errs) {
      return $author$project$Gen$CodeGen$Generate$onFailureSend(errs);
    };
    var $author$project$Gen$CodeGen$Generate$onSuccessSend = _Platform_outgoingPort("onSuccessSend", $elm$json$Json$Encode$list(function($) {
      return $elm$json$Json$Encode$object(_List_fromArray([
        _Utils_Tuple2("contents", $elm$json$Json$Encode$string($.cv)),
        _Utils_Tuple2("path", $elm$json$Json$Encode$string($.f_)),
        _Utils_Tuple2("warnings", $elm$json$Json$Encode$list(function($2) {
          return $elm$json$Json$Encode$object(_List_fromArray([
            _Utils_Tuple2("declaration", $elm$json$Json$Encode$string($2.iw)),
            _Utils_Tuple2("warning", $elm$json$Json$Encode$string($2.jE))
          ]));
        })($.h0))
      ]));
    }));
    var $author$project$Gen$CodeGen$Generate$files = function(list) {
      return $author$project$Gen$CodeGen$Generate$onSuccessSend(list);
    };
    var $elm$core$Platform$Cmd$batch = _Platform_batch;
    var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
    var $elm$core$Platform$Sub$batch = _Platform_batch;
    var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
    var $elm$core$Platform$worker = _Platform_worker;
    var $author$project$Gen$CodeGen$Generate$fromJson = F2(function(decoder, f) {
      return $elm$core$Platform$worker({
        J: function(flags) {
          var _v0 = A2($elm$json$Json$Decode$decodeValue, decoder, flags);
          if (!_v0.$) {
            var input = _v0.a;
            return _Utils_Tuple2(0, $author$project$Gen$CodeGen$Generate$files(f(input)));
          } else {
            var e = _v0.a;
            return _Utils_Tuple2(0, $author$project$Gen$CodeGen$Generate$error(_List_fromArray([
              {
                cP: $elm$json$Json$Decode$errorToString(e),
                jz: "Error decoding flags"
              }
            ])));
          }
        },
        bk: function(_v1) {
          return $elm$core$Platform$Sub$none;
        },
        aJ: F2(function(_v2, model) {
          return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
        })
      });
    });
    var $elm$core$String$length = _String_length;
    var $elm$core$String$dropLeft = F2(function(n, string) {
      return n < 1 ? string : A3($elm$core$String$slice, n, $elm$core$String$length(string), string);
    });
    var $elm$core$String$left = F2(function(n, string) {
      return n < 1 ? "" : A3($elm$core$String$slice, 0, n, string);
    });
    var $elm$core$String$toUpper = _String_toUpper;
    var $author$project$Extra$String$capitalize = function(str) {
      var top = A2($elm$core$String$left, 1, str);
      var remain = A2($elm$core$String$dropLeft, 1, str);
      return _Utils_ap($elm$core$String$toUpper(top), remain);
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$Declaration = function(a) {
      return { $: 0, a };
    };
    var $stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration = function(a) {
      return { $: 0, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$NotExposed = { $: 0 };
    var $elm$core$Result$andThen = F2(function(callback, result) {
      if (!result.$) {
        var value = result.a;
        return callback(value);
      } else {
        var msg = result.a;
        return $elm$core$Result$Err(msg);
      }
    });
    var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation = F2(function(a, b) {
      return { $: 6, a, b };
    });
    var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord = F2(function(a, b) {
      return { $: 5, a, b };
    });
    var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType = function(a) {
      return { $: 0, a };
    };
    var $stil4m$elm_syntax$Elm$Syntax$Node$Node = F2(function(a, b) {
      return { $: 0, a, b };
    });
    var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record = function(a) {
      return { $: 4, a };
    };
    var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled = function(a) {
      return { $: 3, a };
    };
    var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed = F2(function(a, b) {
      return { $: 1, a, b };
    });
    var $mdgriffith$elm_codegen$Internal$Index$getImport = F2(function(_v0, importedAs) {
      var maybeModName = _v0.a;
      if (!maybeModName.$) {
        var modName = maybeModName.a;
        return _Utils_eq(modName, importedAs) ? _List_Nil : importedAs;
      } else {
        return importedAs;
      }
    });
    var $mdgriffith$elm_codegen$Internal$Clean$adjustQualification = F2(function(index, _v0) {
      var range = _v0.a;
      var _v1 = _v0.b;
      var modName = _v1.a;
      var name = _v1.b;
      return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, _Utils_Tuple2(A2($mdgriffith$elm_codegen$Internal$Index$getImport, index, modName), name));
    });
    var $stil4m$elm_syntax$Elm$Syntax$Node$map = F2(function(f, _v0) {
      var r = _v0.a;
      var a = _v0.b;
      return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, r, f(a));
    });
    var $elm$core$Tuple$mapSecond = F2(function(func, _v0) {
      var x = _v0.a;
      var y = _v0.b;
      return _Utils_Tuple2(x, func(y));
    });
    var $mdgriffith$elm_codegen$Internal$Clean$doRename = F3(function(index, dict, ann) {
      switch (ann.$) {
        case 0:
          var generic = ann.a;
          var _v1 = A2($elm$core$Dict$get, generic, dict);
          if (_v1.$ === 1) {
            return ann;
          } else {
            var renamed = _v1.a;
            return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(renamed);
          }
        case 1:
          var name = ann.a;
          var nodedVars = ann.b;
          return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, A2($mdgriffith$elm_codegen$Internal$Clean$adjustQualification, index, name), A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$map(A2($mdgriffith$elm_codegen$Internal$Clean$doRename, index, dict)), nodedVars));
        case 2:
          return ann;
        case 3:
          var nodedVars = ann.a;
          return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$map(A2($mdgriffith$elm_codegen$Internal$Clean$doRename, index, dict)), nodedVars));
        case 4:
          var record = ann.a;
          return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$map($elm$core$Tuple$mapSecond($stil4m$elm_syntax$Elm$Syntax$Node$map(A2($mdgriffith$elm_codegen$Internal$Clean$doRename, index, dict)))), record));
        case 5:
          var name = ann.a;
          var _v2 = ann.b;
          var range = _v2.a;
          var record = _v2.b;
          return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord, name, A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Syntax$Node$map($elm$core$Tuple$mapSecond($stil4m$elm_syntax$Elm$Syntax$Node$map(A2($mdgriffith$elm_codegen$Internal$Clean$doRename, index, dict)))), record)));
        default:
          var nodeOne = ann.a;
          var nodeTwo = ann.b;
          return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, A2($stil4m$elm_syntax$Elm$Syntax$Node$map, A2($mdgriffith$elm_codegen$Internal$Clean$doRename, index, dict), nodeOne), A2($stil4m$elm_syntax$Elm$Syntax$Node$map, A2($mdgriffith$elm_codegen$Internal$Clean$doRename, index, dict), nodeTwo));
      }
    });
    var $mdgriffith$elm_codegen$Internal$Index$hasModuleName = function(_v0) {
      var maybeModName = _v0.a;
      if (!maybeModName.$) {
        return true;
      } else {
        return false;
      }
    };
    var $elm$core$Dict$isEmpty = function(dict) {
      if (dict.$ === -2) {
        return true;
      } else {
        return false;
      }
    };
    var $elm$core$Dict$Black = 1;
    var $elm$core$Dict$RBNode_elm_builtin = F5(function(a, b, c, d, e) {
      return { $: -1, a, b, c, d, e };
    });
    var $elm$core$Dict$Red = 0;
    var $elm$core$Dict$balance = F5(function(color, key, value, left, right) {
      if (right.$ === -1 && !right.a) {
        var _v1 = right.a;
        var rK = right.b;
        var rV = right.c;
        var rLeft = right.d;
        var rRight = right.e;
        if (left.$ === -1 && !left.a) {
          var _v3 = left.a;
          var lK = left.b;
          var lV = left.c;
          var lLeft = left.d;
          var lRight = left.e;
          return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight), A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
        } else {
          return A5($elm$core$Dict$RBNode_elm_builtin, color, rK, rV, A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft), rRight);
        }
      } else {
        if (left.$ === -1 && !left.a && left.d.$ === -1 && !left.d.a) {
          var _v5 = left.a;
          var lK = left.b;
          var lV = left.c;
          var _v6 = left.d;
          var _v7 = _v6.a;
          var llK = _v6.b;
          var llV = _v6.c;
          var llLeft = _v6.d;
          var llRight = _v6.e;
          var lRight = left.e;
          return A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight), A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
        } else {
          return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
        }
      }
    });
    var $elm$core$Dict$insertHelp = F3(function(key, value, dict) {
      if (dict.$ === -2) {
        return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
      } else {
        var nColor = dict.a;
        var nKey = dict.b;
        var nValue = dict.c;
        var nLeft = dict.d;
        var nRight = dict.e;
        var _v1 = A2($elm$core$Basics$compare, key, nKey);
        switch (_v1) {
          case 0:
            return A5($elm$core$Dict$balance, nColor, nKey, nValue, A3($elm$core$Dict$insertHelp, key, value, nLeft), nRight);
          case 1:
            return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
          default:
            return A5($elm$core$Dict$balance, nColor, nKey, nValue, nLeft, A3($elm$core$Dict$insertHelp, key, value, nRight));
        }
      }
    });
    var $elm$core$Dict$insert = F3(function(key, value, dict) {
      var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
      if (_v0.$ === -1 && !_v0.a) {
        var _v1 = _v0.a;
        var k = _v0.b;
        var v = _v0.c;
        var l = _v0.d;
        var r = _v0.e;
        return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
      } else {
        var x = _v0;
        return x;
      }
    });
    var $elm$core$Set$insert = F2(function(key, _v0) {
      var dict = _v0;
      return A3($elm$core$Dict$insert, key, 0, dict);
    });
    var $mdgriffith$elm_codegen$Internal$Clean$prepareRename = F2(function(ann, dict) {
      switch (ann.$) {
        case 0:
          var generic = ann.a;
          return A2($elm$core$Set$insert, generic, dict);
        case 1:
          var nodedVars = ann.b;
          return A3($elm$core$List$foldl, F2(function(_v1, d) {
            var tipe = _v1.b;
            return A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, tipe, d);
          }), dict, nodedVars);
        case 2:
          return dict;
        case 3:
          var nodedVars = ann.a;
          return A3($elm$core$List$foldl, F2(function(_v2, d) {
            var tipe = _v2.b;
            return A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, tipe, d);
          }), dict, nodedVars);
        case 4:
          var record = ann.a;
          return A3($elm$core$List$foldl, F2(function(_v3, d) {
            var _v4 = _v3.b;
            var _v5 = _v4.b;
            var field = _v5.b;
            return A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, field, d);
          }), dict, record);
        case 5:
          var _v6 = ann.b;
          var record = _v6.b;
          return A3($elm$core$List$foldl, F2(function(_v7, d) {
            var _v8 = _v7.b;
            var _v9 = _v8.b;
            var field = _v9.b;
            return A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, field, d);
          }), dict, record);
        default:
          var _v10 = ann.a;
          var one = _v10.b;
          var _v11 = ann.b;
          var two = _v11.b;
          return A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, two, A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, one, dict));
      }
    });
    var $mdgriffith$elm_codegen$Internal$Clean$findClean = F3(function(i, name, set) {
      findClean:
        while (true) {
          var newName = !i ? name : _Utils_ap(name, $elm$core$String$fromInt(i));
          if (A2($elm$core$Set$member, newName, set)) {
            var $temp$i = i + 1, $temp$name = name, $temp$set = set;
            i = $temp$i;
            name = $temp$name;
            set = $temp$set;
            continue findClean;
          } else {
            return name;
          }
        }
    });
    var $elm$core$Dict$foldl = F3(function(func, acc, dict) {
      foldl:
        while (true) {
          if (dict.$ === -2) {
            return acc;
          } else {
            var key = dict.b;
            var value = dict.c;
            var left = dict.d;
            var right = dict.e;
            var $temp$func = func, $temp$acc = A3(func, key, value, A3($elm$core$Dict$foldl, func, acc, left)), $temp$dict = right;
            func = $temp$func;
            acc = $temp$acc;
            dict = $temp$dict;
            continue foldl;
          }
        }
    });
    var $elm$core$Set$foldl = F3(function(func, initialState, _v0) {
      var dict = _v0;
      return A3($elm$core$Dict$foldl, F3(function(key, _v1, state) {
        return A2(func, key, state);
      }), initialState, dict);
    });
    var $mdgriffith$elm_codegen$Internal$Clean$sanitized = function(str) {
      var _v0 = A2($elm$core$String$split, "_", str);
      if (!_v0.b) {
        return str;
      } else {
        var top = _v0.a;
        return top;
      }
    };
    var $mdgriffith$elm_codegen$Internal$Clean$verify = function(set) {
      return A3($elm$core$Set$foldl, F2(function(name, gathered) {
        var newName = A3($mdgriffith$elm_codegen$Internal$Clean$findClean, 0, $mdgriffith$elm_codegen$Internal$Clean$sanitized(name), set);
        return A3($elm$core$Dict$insert, name, newName, gathered);
      }), $elm$core$Dict$empty, set);
    };
    var $mdgriffith$elm_codegen$Internal$Clean$clean = F2(function(index, ann) {
      var renames = $mdgriffith$elm_codegen$Internal$Clean$verify(A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, ann, $elm$core$Set$empty));
      return $elm$core$Dict$isEmpty(renames) && !$mdgriffith$elm_codegen$Internal$Index$hasModuleName(index) ? ann : A3($mdgriffith$elm_codegen$Internal$Clean$doRename, index, renames, ann);
    });
    var $elm$core$String$replace = F3(function(before, after, string) {
      return A2($elm$core$String$join, after, A2($elm$core$String$split, before, string));
    });
    var $mdgriffith$elm_codegen$Internal$Format$sanitize = function(str) {
      switch (str) {
        case "in":
          return "in_";
        case "type":
          return "type_";
        case "case":
          return "case_";
        case "let":
          return "let_";
        case "module":
          return "module_";
        case "exposing":
          return "exposing_";
        case "where":
          return "where_";
        case "main":
          return "main_";
        case "port":
          return "port_";
        case "as":
          return "as_";
        case "if":
          return "if_";
        case "import":
          return "import_";
        default:
          return A3($elm$core$String$replace, ".", "", str);
      }
    };
    var $elm$core$String$toLower = _String_toLower;
    var $mdgriffith$elm_codegen$Internal$Format$formatValue = function(str) {
      var formatted = _Utils_ap($elm$core$String$toLower(A2($elm$core$String$left, 1, str)), A2($elm$core$String$dropLeft, 1, str));
      return $mdgriffith$elm_codegen$Internal$Format$sanitize(formatted);
    };
    var $mdgriffith$elm_codegen$Internal$Format$formatDeclarationName = function(str) {
      if (str === "main") {
        return "main";
      } else {
        return $mdgriffith$elm_codegen$Internal$Format$formatValue(str);
      }
    };
    var $elm$core$Result$mapError = F2(function(f, result) {
      if (!result.$) {
        var v = result.a;
        return $elm$core$Result$Ok(v);
      } else {
        var e = result.a;
        return $elm$core$Result$Err(f(e));
      }
    });
    var $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange = {
      aT: { iq: 0, jn: 0 },
      z: { iq: 0, jn: 0 }
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$nodify = function(exp) {
      return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, exp);
    };
    var $elm$core$Bitwise$and = _Bitwise_and;
    var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
    var $elm$core$String$repeatHelp = F3(function(n, chunk, result) {
      return n <= 0 ? result : A3($elm$core$String$repeatHelp, n >> 1, _Utils_ap(chunk, chunk), !(n & 1) ? result : _Utils_ap(result, chunk));
    });
    var $elm$core$String$repeat = F2(function(n, chunk) {
      return A3($elm$core$String$repeatHelp, n, chunk, "");
    });
    var $stil4m$structured_writer$StructuredWriter$asIndent = function(amount) {
      return A2($elm$core$String$repeat, amount, " ");
    };
    var $elm$core$Basics$composeR = F3(function(f, g, x) {
      return g(f(x));
    });
    var $elm$core$String$concat = function(strings) {
      return A2($elm$core$String$join, "", strings);
    };
    var $stil4m$structured_writer$StructuredWriter$writeIndented = F2(function(indent_, w) {
      switch (w.$) {
        case 0:
          var _v1 = w.a;
          var pre = _v1.a;
          var sep = _v1.b;
          var post = _v1.c;
          var differentLines = w.b;
          var items = w.c;
          var seperator = differentLines ? `
` + ($stil4m$structured_writer$StructuredWriter$asIndent(indent_) + sep) : sep;
          return $elm$core$String$concat(_List_fromArray([
            pre,
            A2($elm$core$String$join, seperator, A2($elm$core$List$map, A2($elm$core$Basics$composeR, $elm$core$Basics$identity, $stil4m$structured_writer$StructuredWriter$writeIndented(indent_)), items)),
            post
          ]));
        case 1:
          var items = w.a;
          return A2($elm$core$String$join, `
` + $stil4m$structured_writer$StructuredWriter$asIndent(indent_), A2($elm$core$List$concatMap, A2($elm$core$Basics$composeR, $stil4m$structured_writer$StructuredWriter$writeIndented(0), $elm$core$String$split(`
`)), items));
        case 2:
          var s = w.a;
          return s;
        case 4:
          var n = w.a;
          var next = w.b;
          return _Utils_ap($stil4m$structured_writer$StructuredWriter$asIndent(n + indent_), A2($stil4m$structured_writer$StructuredWriter$writeIndented, n + indent_, next));
        case 5:
          var items = w.a;
          return A2($elm$core$String$join, " ", A2($elm$core$List$map, $stil4m$structured_writer$StructuredWriter$writeIndented(indent_), items));
        case 6:
          var items = w.a;
          return $elm$core$String$concat(A2($elm$core$List$map, $stil4m$structured_writer$StructuredWriter$writeIndented(indent_), items));
        default:
          var x = w.a;
          var y = w.b;
          return _Utils_ap(A2($stil4m$structured_writer$StructuredWriter$writeIndented, indent_, x), A2($stil4m$structured_writer$StructuredWriter$writeIndented, indent_, y));
      }
    });
    var $stil4m$structured_writer$StructuredWriter$write = $stil4m$structured_writer$StructuredWriter$writeIndented(0);
    var $stil4m$elm_syntax$Elm$Writer$write = $stil4m$structured_writer$StructuredWriter$write;
    var $stil4m$structured_writer$StructuredWriter$Sep = F3(function(a, b, c) {
      return { $: 0, a, b, c };
    });
    var $stil4m$structured_writer$StructuredWriter$bracesComma = $stil4m$structured_writer$StructuredWriter$Sep(_Utils_Tuple3("{", ", ", "}"));
    var $stil4m$structured_writer$StructuredWriter$Joined = function(a) {
      return { $: 6, a };
    };
    var $stil4m$structured_writer$StructuredWriter$join = $stil4m$structured_writer$StructuredWriter$Joined;
    var $stil4m$structured_writer$StructuredWriter$parensComma = $stil4m$structured_writer$StructuredWriter$Sep(_Utils_Tuple3("(", ", ", ")"));
    var $elm$core$String$contains = _String_contains;
    var $stil4m$structured_writer$StructuredWriter$Str = function(a) {
      return { $: 2, a };
    };
    var $stil4m$structured_writer$StructuredWriter$string = $stil4m$structured_writer$StructuredWriter$Str;
    var $stil4m$elm_syntax$Elm$Writer$parensIfContainsSpaces = function(w) {
      return A2($elm$core$String$contains, " ", $stil4m$structured_writer$StructuredWriter$write(w)) ? $stil4m$structured_writer$StructuredWriter$join(_List_fromArray([
        $stil4m$structured_writer$StructuredWriter$string("("),
        w,
        $stil4m$structured_writer$StructuredWriter$string(")")
      ])) : w;
    };
    var $elm$core$Tuple$second = function(_v0) {
      var y = _v0.b;
      return y;
    };
    var $stil4m$structured_writer$StructuredWriter$sepByComma = $stil4m$structured_writer$StructuredWriter$Sep(_Utils_Tuple3("", ", ", ""));
    var $stil4m$structured_writer$StructuredWriter$Spaced = function(a) {
      return { $: 5, a };
    };
    var $stil4m$structured_writer$StructuredWriter$spaced = $stil4m$structured_writer$StructuredWriter$Spaced;
    var $stil4m$elm_syntax$Elm$Syntax$Node$value = function(_v0) {
      var v = _v0.b;
      return v;
    };
    var $stil4m$elm_syntax$Elm$Writer$writeRecordField = function(_v4) {
      var _v5 = _v4.b;
      var name = _v5.a;
      var ref = _v5.b;
      return $stil4m$structured_writer$StructuredWriter$spaced(_List_fromArray([
        $stil4m$structured_writer$StructuredWriter$string($stil4m$elm_syntax$Elm$Syntax$Node$value(name)),
        $stil4m$structured_writer$StructuredWriter$string(":"),
        $stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(ref)
      ]));
    };
    var $stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation = function(_v0) {
      var typeAnnotation = _v0.b;
      switch (typeAnnotation.$) {
        case 0:
          var s = typeAnnotation.a;
          return $stil4m$structured_writer$StructuredWriter$string(s);
        case 1:
          var moduleNameAndName = typeAnnotation.a;
          var args = typeAnnotation.b;
          var moduleName = $stil4m$elm_syntax$Elm$Syntax$Node$value(moduleNameAndName).a;
          var k = $stil4m$elm_syntax$Elm$Syntax$Node$value(moduleNameAndName).b;
          return $stil4m$structured_writer$StructuredWriter$spaced(A2($elm$core$List$cons, $stil4m$structured_writer$StructuredWriter$string(A2($elm$core$String$join, ".", _Utils_ap(moduleName, _List_fromArray([k])))), A2($elm$core$List$map, A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation, $stil4m$elm_syntax$Elm$Writer$parensIfContainsSpaces), args)));
        case 2:
          return $stil4m$structured_writer$StructuredWriter$string("()");
        case 3:
          var xs = typeAnnotation.a;
          return A2($stil4m$structured_writer$StructuredWriter$parensComma, false, A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation, xs));
        case 4:
          var xs = typeAnnotation.a;
          return A2($stil4m$structured_writer$StructuredWriter$bracesComma, false, A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Writer$writeRecordField, xs));
        case 5:
          var name = typeAnnotation.a;
          var fields = typeAnnotation.b;
          return $stil4m$structured_writer$StructuredWriter$spaced(_List_fromArray([
            $stil4m$structured_writer$StructuredWriter$string("{"),
            $stil4m$structured_writer$StructuredWriter$string($stil4m$elm_syntax$Elm$Syntax$Node$value(name)),
            $stil4m$structured_writer$StructuredWriter$string("|"),
            A2($stil4m$structured_writer$StructuredWriter$sepByComma, false, A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Writer$writeRecordField, $stil4m$elm_syntax$Elm$Syntax$Node$value(fields))),
            $stil4m$structured_writer$StructuredWriter$string("}")
          ]));
        default:
          var left = typeAnnotation.a;
          var right = typeAnnotation.b;
          var addParensForSubTypeAnnotation = function(type_) {
            if (type_.b.$ === 6) {
              var _v3 = type_.b;
              return $stil4m$structured_writer$StructuredWriter$join(_List_fromArray([
                $stil4m$structured_writer$StructuredWriter$string("("),
                $stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(type_),
                $stil4m$structured_writer$StructuredWriter$string(")")
              ]));
            } else {
              return $stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(type_);
            }
          };
          return $stil4m$structured_writer$StructuredWriter$spaced(_List_fromArray([
            addParensForSubTypeAnnotation(left),
            $stil4m$structured_writer$StructuredWriter$string("->"),
            addParensForSubTypeAnnotation(right)
          ]));
      }
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$inferenceErrorToString = function(inf) {
      switch (inf.$) {
        case 1:
          var str = inf.a;
          return "Todo " + str;
        case 0:
          var one = inf.a;
          var two = inf.b;
          return `There are multiple different types in a list: 

` + ("    " + ($stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(one))) + (`

    ` + $stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(two))))));
        case 11:
          return "Mismatched record update";
        case 2:
          return "Case statement is empty";
        case 3:
          var fn = inf.a;
          var args = inf.b;
          return `The following is being called as a function

    ` + ($stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(fn))) + (`

with these arguments:

    ` + (A2($elm$core$String$join, " -> ", A2($elm$core$List$map, function(arg) {
            return $stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(arg)));
          }, args)) + `

but that's wrong, right?`)));
        case 5:
          var fieldName = inf.a;
          return "There is a duplicate field in a record: " + fieldName;
        case 6:
          return "Case returns different types.";
        case 7:
          var found = inf.a;
          return "I can't find ." + (found.ar + (`, this record only has these fields:

    ` + A2($elm$core$String$join, `
    `, found.iE)));
        case 8:
          var attempting = inf.a;
          return `You're trying to access

    .` + (attempting.ar + (`

but this value isn't a record. It's a

    ` + $stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(attempting.fy)))));
        case 9:
          var attempting = inf.a;
          return `You're trying to access

    .` + (attempting.ar + (`

but this value isn't a record, it's a

    ` + ($stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(attempting.fy))) + `

Is this value supposed to be an alias for a record? If so, check out Elm.alias!`)));
        case 10:
          var details = inf.a;
          return details.iz + " not found, though I was trying to unpack it in a let expression.";
        case 12:
          var type_ = inf.a;
          return $stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + " is not appendable.  Only Strings and Lists are appendable";
        case 13:
          var type_ = inf.a;
          return $stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + " is not appendable.  Only Strings and Lists are appendable";
        case 14:
          var one = inf.a;
          var two = inf.b;
          return `I found

    ` + ($stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(one))) + (`

But I was expecting:

    ` + $stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(two)))));
        default:
          return "Different lists of type variables";
      }
    };
    var $mdgriffith$elm_codegen$Elm$renderError = function(err) {
      if (!err.b) {
        return "";
      } else {
        return A2($elm$core$String$join, `

`, A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$inferenceErrorToString, err));
      }
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$isAppendable = function(annotation) {
      _v0$2:
        while (true) {
          if (annotation.$ === 1 && !annotation.a.b.a.b) {
            switch (annotation.a.b.b) {
              case "String":
                var _v1 = annotation.a;
                var _v2 = _v1.b;
                return true;
              case "List":
                if (annotation.b.b && !annotation.b.b.b) {
                  var _v3 = annotation.a;
                  var _v4 = _v3.b;
                  var _v5 = annotation.b;
                  return true;
                } else {
                  break _v0$2;
                }
              default:
                break _v0$2;
            }
          } else {
            break _v0$2;
          }
        }
      return false;
    };
    var $elm$core$List$any = F2(function(isOkay, list) {
      any:
        while (true) {
          if (!list.b) {
            return false;
          } else {
            var x = list.a;
            var xs = list.b;
            if (isOkay(x)) {
              return true;
            } else {
              var $temp$isOkay = isOkay, $temp$list = xs;
              isOkay = $temp$isOkay;
              list = $temp$list;
              continue any;
            }
          }
        }
    });
    var $elm$core$Basics$composeL = F3(function(g, f, x) {
      return g(f(x));
    });
    var $elm$core$List$all = F2(function(isOkay, list) {
      return !A2($elm$core$List$any, A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay), list);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$denode = $stil4m$elm_syntax$Elm$Syntax$Node$value;
    var $mdgriffith$elm_codegen$Internal$Compiler$isComparable = function(annotation) {
      isComparable:
        while (true) {
          _v0$6:
            while (true) {
              switch (annotation.$) {
                case 1:
                  if (annotation.a.b.a.b) {
                    if (annotation.a.b.a.a === "Char" && !annotation.a.b.a.b.b && annotation.a.b.b === "Char") {
                      var _v5 = annotation.a;
                      var _v6 = _v5.b;
                      var _v7 = _v6.a;
                      return true;
                    } else {
                      break _v0$6;
                    }
                  } else {
                    switch (annotation.a.b.b) {
                      case "Int":
                        var _v1 = annotation.a;
                        var _v2 = _v1.b;
                        return true;
                      case "Float":
                        var _v3 = annotation.a;
                        var _v4 = _v3.b;
                        return true;
                      case "String":
                        var _v8 = annotation.a;
                        var _v9 = _v8.b;
                        return true;
                      case "List":
                        if (annotation.b.b && !annotation.b.b.b) {
                          var _v10 = annotation.a;
                          var _v11 = _v10.b;
                          var _v12 = annotation.b;
                          var _v13 = _v12.a;
                          var inner = _v13.b;
                          var $temp$annotation = inner;
                          annotation = $temp$annotation;
                          continue isComparable;
                        } else {
                          break _v0$6;
                        }
                      default:
                        break _v0$6;
                    }
                  }
                case 3:
                  var innerList = annotation.a;
                  return A2($elm$core$List$all, A2($elm$core$Basics$composeL, $mdgriffith$elm_codegen$Internal$Compiler$isComparable, $mdgriffith$elm_codegen$Internal$Compiler$denode), innerList);
                default:
                  break _v0$6;
              }
            }
          return false;
        }
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$isNumber = function(annotation) {
      _v0$2:
        while (true) {
          if (annotation.$ === 1 && !annotation.a.b.a.b) {
            switch (annotation.a.b.b) {
              case "Int":
                var _v1 = annotation.a;
                var _v2 = _v1.b;
                return true;
              case "Float":
                var _v3 = annotation.a;
                var _v4 = _v3.b;
                return true;
              default:
                break _v0$2;
            }
          } else {
            break _v0$2;
          }
        }
      return false;
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$checkRestrictions = F2(function(restrictions, type_) {
      switch (restrictions.$) {
        case 0:
          return $elm$core$Result$Ok(type_);
        case 5:
          var constraints = restrictions.a;
          return $elm$core$Result$Err($stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + (" needs to be: " + (A2($elm$core$String$join, ", ", A2($elm$core$List$concatMap, function(constraint) {
            switch (constraint.$) {
              case 0:
                return _List_Nil;
              case 5:
                return _List_Nil;
              case 1:
                return _List_fromArray(["a number"]);
              case 3:
                return _List_fromArray(["comparable"]);
              case 2:
                return _List_fromArray(["appendable"]);
              default:
                return _List_fromArray(["appendable and comparable"]);
            }
          }, constraints)) + `

but that's impossible!  Or Elm Codegen's s typechecker is off.`)));
        case 1:
          return $mdgriffith$elm_codegen$Internal$Compiler$isNumber(type_) ? $elm$core$Result$Ok(type_) : $elm$core$Result$Err($stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + " is not a number");
        case 3:
          return $mdgriffith$elm_codegen$Internal$Compiler$isComparable(type_) ? $elm$core$Result$Ok(type_) : $elm$core$Result$Err($stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + " is not comparable.  Only Ints, Floats, Chars, Strings and Lists and Tuples of those things are comparable.");
        case 2:
          return $mdgriffith$elm_codegen$Internal$Compiler$isAppendable(type_) ? $elm$core$Result$Ok(type_) : $elm$core$Result$Err($stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + " is not appendable.  Only Strings and Lists are appendable.");
        default:
          return $mdgriffith$elm_codegen$Internal$Compiler$isComparable(type_) || $mdgriffith$elm_codegen$Internal$Compiler$isAppendable(type_) ? $elm$core$Result$Ok(type_) : $elm$core$Result$Err($stil4m$elm_syntax$Elm$Writer$write($stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation($mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + " is not appendable/comparable.  Only Strings and Lists are allowed here.");
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$NoRestrictions = { $: 0 };
    var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit = { $: 2 };
    var $mdgriffith$elm_codegen$Internal$Compiler$IsAppendable = { $: 2 };
    var $mdgriffith$elm_codegen$Internal$Compiler$IsAppendableComparable = { $: 4 };
    var $mdgriffith$elm_codegen$Internal$Compiler$IsComparable = { $: 3 };
    var $mdgriffith$elm_codegen$Internal$Compiler$IsNumber = { $: 1 };
    var $elm$core$String$startsWith = _String_startsWith;
    var $mdgriffith$elm_codegen$Internal$Compiler$nameToRestrictions = function(name) {
      return A2($elm$core$String$startsWith, "number", name) ? $mdgriffith$elm_codegen$Internal$Compiler$IsNumber : A2($elm$core$String$startsWith, "comparable", name) ? $mdgriffith$elm_codegen$Internal$Compiler$IsComparable : A2($elm$core$String$startsWith, "appendable", name) ? $mdgriffith$elm_codegen$Internal$Compiler$IsAppendable : A2($elm$core$String$startsWith, "compappend", name) ? $mdgriffith$elm_codegen$Internal$Compiler$IsAppendableComparable : $mdgriffith$elm_codegen$Internal$Compiler$NoRestrictions;
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted = function(a) {
      return { $: 5, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$restrictFurther = F2(function(restriction, newRestriction) {
      switch (restriction.$) {
        case 0:
          return newRestriction;
        case 5:
          var constraints = restriction.a;
          switch (newRestriction.$) {
            case 5:
              var newConstraints = newRestriction.a;
              return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(_Utils_ap(constraints, newConstraints));
            case 0:
              return restriction;
            default:
              return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(A2($elm$core$List$cons, newRestriction, constraints));
          }
        case 1:
          switch (newRestriction.$) {
            case 1:
              return newRestriction;
            case 0:
              return restriction;
            case 5:
              var constraints = newRestriction.a;
              return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(A2($elm$core$List$cons, restriction, constraints));
            default:
              return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(_List_fromArray([restriction, newRestriction]));
          }
        case 3:
          switch (newRestriction.$) {
            case 0:
              return restriction;
            case 4:
              return newRestriction;
            case 3:
              return newRestriction;
            case 5:
              var constraints = newRestriction.a;
              return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(A2($elm$core$List$cons, restriction, constraints));
            default:
              return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(_List_fromArray([restriction, newRestriction]));
          }
        case 2:
          switch (newRestriction.$) {
            case 0:
              return restriction;
            case 4:
              return newRestriction;
            case 3:
              return newRestriction;
            case 5:
              var constraints = newRestriction.a;
              return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(A2($elm$core$List$cons, restriction, constraints));
            default:
              return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(_List_fromArray([restriction, newRestriction]));
          }
        default:
          switch (newRestriction.$) {
            case 0:
              return restriction;
            case 4:
              return newRestriction;
            case 3:
              return newRestriction;
            case 2:
              return newRestriction;
            case 5:
              var constraints = newRestriction.a;
              return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(A2($elm$core$List$cons, restriction, constraints));
            default:
              return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(_List_fromArray([restriction, newRestriction]));
          }
      }
    });
    var $elm$core$Maybe$withDefault = F2(function(_default, maybe) {
      if (!maybe.$) {
        var value = maybe.a;
        return value;
      } else {
        return _default;
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$getRestrictionsHelper = F3(function(existingRestrictions, notation, cache) {
      getRestrictionsHelper:
        while (true) {
          switch (notation.$) {
            case 6:
              return existingRestrictions;
            case 0:
              var name = notation.a;
              var $temp$existingRestrictions = A2($mdgriffith$elm_codegen$Internal$Compiler$restrictFurther, existingRestrictions, $mdgriffith$elm_codegen$Internal$Compiler$nameToRestrictions(name)), $temp$notation = A2($elm$core$Maybe$withDefault, $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit, A2($elm$core$Dict$get, name, cache)), $temp$cache = cache;
              existingRestrictions = $temp$existingRestrictions;
              notation = $temp$notation;
              cache = $temp$cache;
              continue getRestrictionsHelper;
            case 1:
              return existingRestrictions;
            case 2:
              return existingRestrictions;
            case 3:
              return existingRestrictions;
            case 4:
              return existingRestrictions;
            default:
              return existingRestrictions;
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$getRestrictions = F2(function(notation, cache) {
      return A3($mdgriffith$elm_codegen$Internal$Compiler$getRestrictionsHelper, $mdgriffith$elm_codegen$Internal$Compiler$NoRestrictions, notation, cache);
    });
    var $mdgriffith$elm_codegen$Result$Extra$combineMapHelper = F3(function(f, acc, list) {
      combineMapHelper:
        while (true) {
          if (!list.b) {
            return $elm$core$Result$Ok($elm$core$List$reverse(acc));
          } else {
            var h = list.a;
            var t = list.b;
            var _v1 = f(h);
            if (_v1.$ === 1) {
              var e = _v1.a;
              return $elm$core$Result$Err(e);
            } else {
              var fh = _v1.a;
              var $temp$f = f, $temp$acc = A2($elm$core$List$cons, fh, acc), $temp$list = t;
              f = $temp$f;
              acc = $temp$acc;
              list = $temp$list;
              continue combineMapHelper;
            }
          }
        }
    });
    var $mdgriffith$elm_codegen$Result$Extra$combineMap = F2(function(f, list) {
      return A3($mdgriffith$elm_codegen$Result$Extra$combineMapHelper, f, _List_Nil, list);
    });
    var $elm$core$Result$map = F2(function(func, ra) {
      if (!ra.$) {
        var a = ra.a;
        return $elm$core$Result$Ok(func(a));
      } else {
        var e = ra.a;
        return $elm$core$Result$Err(e);
      }
    });
    var $elm$core$Result$map2 = F3(function(func, ra, rb) {
      if (ra.$ === 1) {
        var x = ra.a;
        return $elm$core$Result$Err(x);
      } else {
        var a = ra.a;
        if (rb.$ === 1) {
          var x = rb.a;
          return $elm$core$Result$Err(x);
        } else {
          var b = rb.a;
          return $elm$core$Result$Ok(A2(func, a, b));
        }
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$resolveVariableList = F4(function(visited, cache, nodes, processed) {
      resolveVariableList:
        while (true) {
          if (!nodes.b) {
            return $elm$core$Result$Ok($elm$core$List$reverse(processed));
          } else {
            var _v13 = nodes.a;
            var coords = _v13.a;
            var top = _v13.b;
            var remain = nodes.b;
            var _v14 = A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, visited, cache, top);
            if (!_v14.$) {
              var resolved = _v14.a;
              var $temp$visited = visited, $temp$cache = cache, $temp$nodes = remain, $temp$processed = A2($elm$core$List$cons, A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, coords, resolved), processed);
              visited = $temp$visited;
              cache = $temp$cache;
              nodes = $temp$nodes;
              processed = $temp$processed;
              continue resolveVariableList;
            } else {
              var err = _v14.a;
              return $elm$core$Result$Err(err);
            }
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$resolveVariables = F3(function(visited, cache, annotation) {
      resolveVariables:
        while (true) {
          switch (annotation.$) {
            case 6:
              var _v1 = annotation.a;
              var oneCoords = _v1.a;
              var one = _v1.b;
              var _v2 = annotation.b;
              var twoCoords = _v2.a;
              var two = _v2.b;
              return A3($elm$core$Result$map2, F2(function(oneResolved, twoResolved) {
                return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, oneCoords, oneResolved), A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, twoCoords, twoResolved));
              }), A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, visited, cache, one), A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, visited, cache, two));
            case 0:
              var name = annotation.a;
              if (A2($elm$core$Set$member, name, visited)) {
                return $elm$core$Result$Err("Infinite type inference loop!  Whoops.  This is an issue with elm-codegen.  If you can report this to the elm-codegen repo, that would be appreciated!");
              } else {
                var _v3 = A2($elm$core$Dict$get, name, cache);
                if (_v3.$ === 1) {
                  return $elm$core$Result$Ok(annotation);
                } else {
                  var newType = _v3.a;
                  var $temp$visited = A2($elm$core$Set$insert, name, visited), $temp$cache = cache, $temp$annotation = newType;
                  visited = $temp$visited;
                  cache = $temp$cache;
                  annotation = $temp$annotation;
                  continue resolveVariables;
                }
              }
            case 1:
              var nodedModuleName = annotation.a;
              var vars = annotation.b;
              return A2($elm$core$Result$map, $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed(nodedModuleName), A4($mdgriffith$elm_codegen$Internal$Compiler$resolveVariableList, visited, cache, vars, _List_Nil));
            case 2:
              return $elm$core$Result$Ok($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit);
            case 3:
              var nodes = annotation.a;
              return A2($elm$core$Result$map, $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled, A4($mdgriffith$elm_codegen$Internal$Compiler$resolveVariableList, visited, cache, nodes, _List_Nil));
            case 4:
              var fields = annotation.a;
              return A2($elm$core$Result$map, $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record, A2($mdgriffith$elm_codegen$Result$Extra$combineMap, function(_v4) {
                var fieldRange = _v4.a;
                var _v5 = _v4.b;
                var name2 = _v5.a;
                var _v6 = _v5.b;
                var fieldTypeRange = _v6.a;
                var fieldType = _v6.b;
                return A2($elm$core$Result$andThen, function(resolvedField) {
                  var restrictions = A2($mdgriffith$elm_codegen$Internal$Compiler$getRestrictions, annotation, cache);
                  return A2($elm$core$Result$map, function(_v7) {
                    return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fieldRange, _Utils_Tuple2(name2, A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fieldTypeRange, resolvedField)));
                  }, A2($mdgriffith$elm_codegen$Internal$Compiler$checkRestrictions, restrictions, resolvedField));
                }, A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, visited, cache, fieldType));
              }, fields));
            default:
              var baseName = annotation.a;
              var _v8 = annotation.b;
              var recordNode = _v8.a;
              var fields = _v8.b;
              return A2($elm$core$Result$map, function(newFields) {
                return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord, baseName, A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, recordNode, newFields));
              }, A2($mdgriffith$elm_codegen$Result$Extra$combineMap, function(_v9) {
                var fieldRange = _v9.a;
                var _v10 = _v9.b;
                var name2 = _v10.a;
                var _v11 = _v10.b;
                var fieldTypeRange = _v11.a;
                var fieldType = _v11.b;
                return A2($elm$core$Result$map, function(resolvedField) {
                  return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fieldRange, _Utils_Tuple2(name2, A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fieldTypeRange, resolvedField)));
                }, A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, visited, cache, fieldType));
              }, fields));
          }
        }
    });
    var $elm$core$Set$fromList = function(list) {
      return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper = function(ann) {
      switch (ann.$) {
        case 0:
          var str = ann.a;
          return _List_fromArray([str]);
        case 1:
          var anns = ann.b;
          return A2($elm$core$List$concatMap, A2($elm$core$Basics$composeL, $mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper, $mdgriffith$elm_codegen$Internal$Compiler$denode), anns);
        case 2:
          return _List_Nil;
        case 3:
          var tupled = ann.a;
          return A2($elm$core$List$concatMap, A2($elm$core$Basics$composeL, $mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper, $mdgriffith$elm_codegen$Internal$Compiler$denode), tupled);
        case 4:
          var recordDefinition = ann.a;
          return A2($elm$core$List$concatMap, function(_v1) {
            var _v2 = _v1.b;
            var _v3 = _v2.b;
            var field = _v3.b;
            return $mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper(field);
          }, recordDefinition);
        case 5:
          var _v4 = ann.a;
          var recordName = _v4.b;
          var _v5 = ann.b;
          var recordDefinition = _v5.b;
          return A2($elm$core$List$cons, recordName, A2($elm$core$List$concatMap, function(_v6) {
            var _v7 = _v6.b;
            var _v8 = _v7.b;
            var field = _v8.b;
            return $mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper(field);
          }, recordDefinition));
        default:
          var _v9 = ann.a;
          var one = _v9.b;
          var _v10 = ann.b;
          var two = _v10.b;
          return A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper, _List_fromArray([one, two]));
      }
    };
    var $elm$core$Basics$neq = _Utils_notEqual;
    var $elm$core$List$filter = F2(function(isGood, list) {
      return A3($elm$core$List$foldr, F2(function(x, xs) {
        return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
      }), _List_Nil, list);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$simplify = function(fullStr) {
      return A2($elm$core$String$join, "_", A2($elm$core$List$filter, function(piece) {
        return !A2($elm$core$String$all, $elm$core$Char$isDigit, piece);
      }, A2($elm$core$String$split, "_", fullStr)));
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariablesHelper = F3(function(existing, renames, type_) {
      switch (type_.$) {
        case 0:
          var varName = type_.a;
          var _v1 = A2($elm$core$Dict$get, varName, renames);
          if (_v1.$ === 1) {
            var simplified = $mdgriffith$elm_codegen$Internal$Compiler$simplify(varName);
            return A2($elm$core$Set$member, simplified, existing) && !_Utils_eq(varName, simplified) ? _Utils_Tuple2(renames, $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(simplified)) : _Utils_Tuple2(A3($elm$core$Dict$insert, varName, simplified, renames), $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(simplified));
          } else {
            var rename = _v1.a;
            return _Utils_Tuple2(renames, $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(rename));
          }
        case 1:
          var name = type_.a;
          var vars = type_.b;
          var _v2 = A3($elm$core$List$foldl, F2(function(_v3, _v4) {
            var typevar = _v3.b;
            var varUsed = _v4.a;
            var varList = _v4.b;
            var _v5 = A3($mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariablesHelper, existing, varUsed, typevar);
            var oneUsed2 = _v5.a;
            var oneType2 = _v5.b;
            return _Utils_Tuple2(oneUsed2, A2($elm$core$List$cons, $mdgriffith$elm_codegen$Internal$Compiler$nodify(oneType2), varList));
          }), _Utils_Tuple2(renames, _List_Nil), vars);
          var newUsed = _v2.a;
          var newVars = _v2.b;
          return _Utils_Tuple2(newUsed, A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, name, $elm$core$List$reverse(newVars)));
        case 2:
          return _Utils_Tuple2(renames, type_);
        case 3:
          return _Utils_Tuple2(renames, type_);
        case 4:
          return _Utils_Tuple2(renames, type_);
        case 5:
          return _Utils_Tuple2(renames, type_);
        default:
          var _v6 = type_.a;
          var one = _v6.b;
          var _v7 = type_.b;
          var two = _v7.b;
          var _v8 = A3($mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariablesHelper, existing, renames, one);
          var oneUsed = _v8.a;
          var oneType = _v8.b;
          var _v9 = A3($mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariablesHelper, existing, oneUsed, two);
          var twoUsed = _v9.a;
          var twoType = _v9.b;
          return _Utils_Tuple2(twoUsed, A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, $mdgriffith$elm_codegen$Internal$Compiler$nodify(oneType), $mdgriffith$elm_codegen$Internal$Compiler$nodify(twoType)));
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariables = function(type_) {
      var existing = $elm$core$Set$fromList($mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper(type_));
      return A3($mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariablesHelper, existing, $elm$core$Dict$empty, type_).b;
    };
    var $mdgriffith$elm_codegen$Internal$Index$typecheck = function(_v0) {
      var check = _v0.e;
      return check;
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$resolve = F3(function(index, cache, annotation) {
      if ($mdgriffith$elm_codegen$Internal$Index$typecheck(index)) {
        var _v0 = A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, $elm$core$Set$empty, cache, annotation);
        if (!_v0.$) {
          var newAnnotation = _v0.a;
          var restrictions = A2($mdgriffith$elm_codegen$Internal$Compiler$getRestrictions, annotation, cache);
          return A2($mdgriffith$elm_codegen$Internal$Compiler$checkRestrictions, restrictions, $mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariables(newAnnotation));
        } else {
          var err = _v0.a;
          return $elm$core$Result$Err(err);
        }
      } else {
        return $elm$core$Result$Err("Type inference skipped.");
      }
    });
    var $mdgriffith$elm_codegen$Elm$declaration = F2(function(nameStr, _v0) {
      var toBody = _v0;
      var name = $mdgriffith$elm_codegen$Internal$Format$formatDeclarationName(nameStr);
      return $mdgriffith$elm_codegen$Internal$Compiler$Declaration({
        aQ: $elm$core$Maybe$Nothing,
        du: $mdgriffith$elm_codegen$Internal$Compiler$NotExposed,
        c: _List_Nil,
        ag: name,
        _: function(index) {
          var declBody = toBody(index);
          var resolvedType = A2($elm$core$Result$andThen, function(sig) {
            return A3($mdgriffith$elm_codegen$Internal$Compiler$resolve, index, sig.e, sig.hG);
          }, A2($elm$core$Result$mapError, $mdgriffith$elm_codegen$Elm$renderError, declBody.bM));
          var maybeWarning = function() {
            if (!resolvedType.$) {
              var _v4 = declBody.bM;
              if (!_v4.$) {
                return $elm$core$Maybe$Nothing;
              } else {
                if (!_v4.a.b) {
                  return $elm$core$Maybe$Nothing;
                } else {
                  var err = _v4.a;
                  return $elm$core$Maybe$Just({
                    iw: name,
                    jE: $mdgriffith$elm_codegen$Elm$renderError(err)
                  });
                }
              }
            } else {
              if (resolvedType.a === "") {
                return $elm$core$Maybe$Nothing;
              } else {
                var err = resolvedType.a;
                return $elm$core$Maybe$Just({ iw: name, jE: err });
              }
            }
          }();
          return {
            X: declBody.c,
            iw: $stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration({
              iw: function() {
                var _v1 = declBody.iH;
                if (_v1.$ === 17) {
                  var lam = _v1.a;
                  return $mdgriffith$elm_codegen$Internal$Compiler$nodify({
                    aL: lam.G,
                    iH: lam.iH,
                    ag: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name)
                  });
                } else {
                  return $mdgriffith$elm_codegen$Internal$Compiler$nodify({
                    aL: _List_Nil,
                    iH: $mdgriffith$elm_codegen$Internal$Compiler$nodify(declBody.iH),
                    ag: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name)
                  });
                }
              }(),
              aR: $elm$core$Maybe$Nothing,
              jq: function() {
                var _v2 = _Utils_Tuple2(declBody.bM, resolvedType);
                if (!_v2.a.$ && !_v2.b.$) {
                  if (!_v2.b.a.$) {
                    return $elm$core$Maybe$Nothing;
                  } else {
                    var finalType = _v2.b.a;
                    return $elm$core$Maybe$Just($mdgriffith$elm_codegen$Internal$Compiler$nodify({
                      ag: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name),
                      aH: $mdgriffith$elm_codegen$Internal$Compiler$nodify(A2($mdgriffith$elm_codegen$Internal$Clean$clean, index, finalType))
                    }));
                  }
                } else {
                  return $elm$core$Maybe$Nothing;
                }
              }()
            }),
            jE: maybeWarning
          };
        }
      });
    });
    var $stil4m$elm_syntax$Elm$Syntax$Exposing$All = function(a) {
      return { $: 0, a };
    };
    var $mdgriffith$elm_codegen$Internal$Render$Everything = 0;
    var $stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit = function(a) {
      return { $: 1, a };
    };
    var $mdgriffith$elm_codegen$Internal$Comments$Markdown = function(a) {
      return { $: 0, a };
    };
    var $mdgriffith$elm_codegen$Internal$Render$Normal = { $: 0 };
    var $stil4m$elm_syntax$Elm$Syntax$Module$NormalModule = function(a) {
      return { $: 0, a };
    };
    var $mdgriffith$elm_codegen$Internal$Render$OnlyGroups = 1;
    var $stil4m$elm_syntax$Elm$Syntax$Module$PortModule = function(a) {
      return { $: 1, a };
    };
    var $mdgriffith$elm_codegen$Internal$Comments$Comment = $elm$core$Basics$identity;
    var $mdgriffith$elm_codegen$Internal$Comments$addPart = F2(function(_v0, part) {
      var parts = _v0;
      return A2($elm$core$List$cons, part, parts);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$fullModName = function(name) {
      return A2($elm$core$String$join, ".", name);
    };
    var $elm$core$List$sortBy = _List_sortBy;
    var $mdgriffith$elm_codegen$Internal$Render$dedupImports = function(mods) {
      return A2($elm$core$List$sortBy, $mdgriffith$elm_codegen$Internal$Compiler$fullModName, A3($elm$core$List$foldl, F2(function(mod, _v0) {
        var set = _v0.a;
        var gathered = _v0.b;
        var stringName = $mdgriffith$elm_codegen$Internal$Compiler$fullModName(mod);
        return A2($elm$core$Set$member, stringName, set) ? _Utils_Tuple2(set, gathered) : _Utils_Tuple2(A2($elm$core$Set$insert, stringName, set), A2($elm$core$List$cons, mod, gathered));
      }), _Utils_Tuple2($elm$core$Set$empty, _List_Nil), mods).b);
    };
    var $mdgriffith$elm_codegen$Internal$Comments$emptyComment = _List_Nil;
    var $mdgriffith$elm_codegen$Internal$Render$RenderingDocsLine = function(a) {
      return { $: 1, a };
    };
    var $mdgriffith$elm_codegen$Internal$Render$exposedGroupToMarkdown = F4(function(docMode, groups, mode, rendered) {
      exposedGroupToMarkdown:
        while (true) {
          if (!groups.b) {
            if (!mode.$) {
              return rendered;
            } else {
              return rendered;
            }
          } else {
            switch (groups.a.$) {
              case 1:
                var docs = groups.a.a;
                var rest = groups.b;
                if (!mode.$) {
                  var separator = $elm$core$String$isEmpty(rendered) ? "" : `

`;
                  var $temp$docMode = docMode, $temp$groups = rest, $temp$mode = mode, $temp$rendered = _Utils_ap(rendered, _Utils_ap(separator, docs));
                  docMode = $temp$docMode;
                  groups = $temp$groups;
                  mode = $temp$mode;
                  rendered = $temp$rendered;
                  continue exposedGroupToMarkdown;
                } else {
                  var $temp$docMode = docMode, $temp$groups = rest, $temp$mode = mode, $temp$rendered = rendered + (`

` + docs);
                  docMode = $temp$docMode;
                  groups = $temp$groups;
                  mode = $temp$mode;
                  rendered = $temp$rendered;
                  continue exposedGroupToMarkdown;
                }
              case 0:
                var exposedName = groups.a.a;
                var rest = groups.b;
                if (!docMode) {
                  if (!mode.$) {
                    if ($elm$core$String$isEmpty(rendered)) {
                      var $temp$docMode = docMode, $temp$groups = rest, $temp$mode = $mdgriffith$elm_codegen$Internal$Render$RenderingDocsLine(1), $temp$rendered = "@docs " + exposedName;
                      docMode = $temp$docMode;
                      groups = $temp$groups;
                      mode = $temp$mode;
                      rendered = $temp$rendered;
                      continue exposedGroupToMarkdown;
                    } else {
                      var $temp$docMode = docMode, $temp$groups = rest, $temp$mode = $mdgriffith$elm_codegen$Internal$Render$RenderingDocsLine(1), $temp$rendered = rendered + (`

@docs ` + exposedName);
                      docMode = $temp$docMode;
                      groups = $temp$groups;
                      mode = $temp$mode;
                      rendered = $temp$rendered;
                      continue exposedGroupToMarkdown;
                    }
                  } else {
                    var docsItemCount = mode.a;
                    if (docsItemCount > 5) {
                      var $temp$docMode = docMode, $temp$groups = rest, $temp$mode = $mdgriffith$elm_codegen$Internal$Render$RenderingDocsLine(1), $temp$rendered = rendered + (`
@docs ` + exposedName);
                      docMode = $temp$docMode;
                      groups = $temp$groups;
                      mode = $temp$mode;
                      rendered = $temp$rendered;
                      continue exposedGroupToMarkdown;
                    } else {
                      var $temp$docMode = docMode, $temp$groups = rest, $temp$mode = $mdgriffith$elm_codegen$Internal$Render$RenderingDocsLine(docsItemCount + 1), $temp$rendered = rendered + (", " + exposedName);
                      docMode = $temp$docMode;
                      groups = $temp$groups;
                      mode = $temp$mode;
                      rendered = $temp$rendered;
                      continue exposedGroupToMarkdown;
                    }
                  }
                } else {
                  var $temp$docMode = docMode, $temp$groups = rest, $temp$mode = mode, $temp$rendered = rendered;
                  docMode = $temp$docMode;
                  groups = $temp$groups;
                  mode = $temp$mode;
                  rendered = $temp$rendered;
                  continue exposedGroupToMarkdown;
                }
              default:
                var groupDecls = groups.a.a;
                var rest = groups.b;
                var separator = $elm$core$String$isEmpty(rendered) ? "" : `

`;
                var renderedSection = A4($mdgriffith$elm_codegen$Internal$Render$exposedGroupToMarkdown, docMode, $elm$core$List$reverse(groupDecls), $mdgriffith$elm_codegen$Internal$Render$Normal, "");
                var $temp$docMode = docMode, $temp$groups = rest, $temp$mode = $mdgriffith$elm_codegen$Internal$Render$Normal, $temp$rendered = _Utils_ap(rendered, _Utils_ap(separator, renderedSection));
                docMode = $temp$docMode;
                groups = $temp$groups;
                mode = $temp$mode;
                rendered = $temp$rendered;
                continue exposedGroupToMarkdown;
            }
          }
        }
    });
    var $elm$core$List$maybeCons = F3(function(f, mx, xs) {
      var _v0 = f(mx);
      if (!_v0.$) {
        var x = _v0.a;
        return A2($elm$core$List$cons, x, xs);
      } else {
        return xs;
      }
    });
    var $elm$core$List$filterMap = F2(function(f, xs) {
      return A3($elm$core$List$foldr, $elm$core$List$maybeCons(f), _List_Nil, xs);
    });
    var $mdgriffith$elm_codegen$Internal$Render$Exposed = function(a) {
      return { $: 0, a };
    };
    var $mdgriffith$elm_codegen$Internal$Render$ExposedGroup = function(a) {
      return { $: 2, a };
    };
    var $mdgriffith$elm_codegen$Internal$Render$ModuleDocs = function(a) {
      return { $: 1, a };
    };
    var $mdgriffith$elm_codegen$Internal$Render$getExposedGroups = F2(function(decl, groups) {
      switch (decl.$) {
        case 0:
          var decDetails = decl.a;
          var _v1 = decDetails.du;
          if (!_v1.$) {
            return groups;
          } else {
            return A2($elm$core$List$cons, $mdgriffith$elm_codegen$Internal$Render$Exposed(decDetails.ag), groups);
          }
        case 2:
          var docs = decl.a;
          return A2($elm$core$List$cons, $mdgriffith$elm_codegen$Internal$Render$ModuleDocs(docs), groups);
        case 4:
          var groupDecls = decl.a;
          return A2($elm$core$List$cons, $mdgriffith$elm_codegen$Internal$Render$ExposedGroup(A3($elm$core$List$foldl, $mdgriffith$elm_codegen$Internal$Render$getExposedGroups, _List_Nil, groupDecls)), groups);
        default:
          return groups;
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$nodeAtLine = F2(function(line, exp) {
      return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, {
        aT: { iq: 0, jn: line },
        z: { iq: 0, jn: line }
      }, exp);
    });
    var $mdgriffith$elm_codegen$Internal$Render$groupExposedItems = F2(function(line, group) {
      return A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$nodeAtLine(line), group.du);
    });
    var $stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose = function(a) {
      return { $: 0, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$builtIn = function(name) {
      _v0$13:
        while (true) {
          if (name.b) {
            if (name.b.b) {
              if (name.a === "Platform" && !name.b.b.b) {
                switch (name.b.a) {
                  case "Sub":
                    var _v1 = name.b;
                    return true;
                  case "Cmd":
                    var _v2 = name.b;
                    return true;
                  default:
                    break _v0$13;
                }
              } else {
                break _v0$13;
              }
            } else {
              switch (name.a) {
                case "List":
                  return true;
                case "Maybe":
                  return true;
                case "String":
                  return true;
                case "Basics":
                  return true;
                case "Char":
                  return true;
                case "Debug":
                  return true;
                case "Tuple":
                  return true;
                case "Result":
                  return true;
                case "Platform":
                  return true;
                case "Sub":
                  return true;
                case "Cmd":
                  return true;
                default:
                  break _v0$13;
              }
            }
          } else {
            break _v0$13;
          }
        }
      return false;
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$findAlias = F2(function(modName, aliases) {
      findAlias:
        while (true) {
          if (!aliases.b) {
            return $elm$core$Maybe$Nothing;
          } else {
            var _v1 = aliases.a;
            var aliasModName = _v1.a;
            var alias = _v1.b;
            var remain = aliases.b;
            if (_Utils_eq(modName, aliasModName)) {
              return $elm$core$Maybe$Just(alias);
            } else {
              var $temp$modName = modName, $temp$aliases = remain;
              modName = $temp$modName;
              aliases = $temp$aliases;
              continue findAlias;
            }
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$isParser = function(name) {
      _v0$2:
        while (true) {
          if (name.b && name.a === "Parser") {
            if (!name.b.b) {
              return true;
            } else {
              if (name.b.a === "Advanced" && !name.b.b.b) {
                var _v1 = name.b;
                return true;
              } else {
                break _v0$2;
              }
            }
          } else {
            break _v0$2;
          }
        }
      return false;
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$isUrlParser = function(name) {
      if (name.b && name.a === "Url" && name.b.b && name.b.a === "Parser" && !name.b.b.b) {
        var _v1 = name.b;
        return true;
      } else {
        return false;
      }
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$makeImport = F3(function(thisModule, aliases, name) {
      if (_Utils_eq(thisModule, name)) {
        return $elm$core$Maybe$Nothing;
      } else {
        if (!name.b) {
          return $elm$core$Maybe$Nothing;
        } else {
          var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$findAlias, name, aliases);
          if (_v1.$ === 1) {
            return $mdgriffith$elm_codegen$Internal$Compiler$builtIn(name) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just({
              dv: $mdgriffith$elm_codegen$Internal$Compiler$isUrlParser(name) ? $elm$core$Maybe$Just($mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(_List_fromArray([
                $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose("</>")),
                $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose("<?>"))
              ])))) : $mdgriffith$elm_codegen$Internal$Compiler$isParser(name) ? $elm$core$Maybe$Just($mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(_List_fromArray([
                $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose("|=")),
                $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose("|."))
              ])))) : $elm$core$Maybe$Nothing,
              fi: $elm$core$Maybe$Nothing,
              a3: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name)
            });
          } else {
            var alias = _v1.a;
            return $elm$core$Maybe$Just({
              dv: $mdgriffith$elm_codegen$Internal$Compiler$isUrlParser(name) ? $elm$core$Maybe$Just($mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(_List_fromArray([
                $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose("</>")),
                $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose("<?>"))
              ])))) : $mdgriffith$elm_codegen$Internal$Compiler$isParser(name) ? $elm$core$Maybe$Just($mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(_List_fromArray([
                $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose("|=")),
                $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose("|."))
              ])))) : $elm$core$Maybe$Nothing,
              fi: $elm$core$Maybe$Just($mdgriffith$elm_codegen$Internal$Compiler$nodify(_List_fromArray([alias]))),
              a3: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name)
            });
          }
        }
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$RenderedBlock = function(a) {
      return { $: 2, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$RenderedComment = function(a) {
      return { $: 1, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$RenderedDecl = function(a) {
      return { $: 0, a };
    };
    var $stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration = function(a) {
      return { $: 1, a };
    };
    var $stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration = function(a) {
      return { $: 2, a };
    };
    var $mdgriffith$elm_codegen$Internal$Render$addDocs = F2(function(maybeDoc, decl) {
      if (maybeDoc.$ === 1) {
        return decl;
      } else {
        var doc = maybeDoc.a;
        switch (decl.$) {
          case 0:
            var func = decl.a;
            return $stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration(_Utils_update(func, {
              aR: $elm$core$Maybe$Just($mdgriffith$elm_codegen$Internal$Compiler$nodify(doc))
            }));
          case 1:
            var typealias = decl.a;
            return $stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration(_Utils_update(typealias, {
              aR: $elm$core$Maybe$Just($mdgriffith$elm_codegen$Internal$Compiler$nodify(doc))
            }));
          case 2:
            var typeDecl = decl.a;
            return $stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration(_Utils_update(typeDecl, {
              aR: $elm$core$Maybe$Just($mdgriffith$elm_codegen$Internal$Compiler$nodify(doc))
            }));
          case 3:
            return decl;
          case 4:
            return decl;
          default:
            return decl;
        }
      }
    });
    var $stil4m$elm_syntax$Elm$Syntax$Exposing$FunctionExpose = function(a) {
      return { $: 1, a };
    };
    var $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose = function(a) {
      return { $: 3, a };
    };
    var $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose = function(a) {
      return { $: 2, a };
    };
    var $mdgriffith$elm_codegen$Internal$Render$addExposed = F4(function(exposePath, exposed, declaration, otherExposes) {
      if (!exposed.$) {
        return otherExposes;
      } else {
        var details = exposed.a;
        var addToExposedCollection = function(_new) {
          if (!otherExposes.b) {
            return _List_fromArray([
              {
                du: _List_fromArray([_new]),
                ek: exposePath
              }
            ]);
          } else {
            var top = otherExposes.a;
            var rest = otherExposes.b;
            return _Utils_eq(top.ek, exposePath) ? A2($elm$core$List$cons, {
              du: A2($elm$core$List$cons, _new, top.du),
              ek: top.ek
            }, rest) : A2($elm$core$List$cons, {
              du: _List_fromArray([_new]),
              ek: exposePath
            }, otherExposes);
          }
        };
        switch (declaration.$) {
          case 0:
            var fn = declaration.a;
            var fnName = $mdgriffith$elm_codegen$Internal$Compiler$denode(function($) {
              return $.ag;
            }($mdgriffith$elm_codegen$Internal$Compiler$denode(fn.iw)));
            return addToExposedCollection($stil4m$elm_syntax$Elm$Syntax$Exposing$FunctionExpose(fnName));
          case 1:
            var synonym = declaration.a;
            var aliasName = $mdgriffith$elm_codegen$Internal$Compiler$denode(synonym.ag);
            return addToExposedCollection($stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose(aliasName));
          case 2:
            var myType = declaration.a;
            var typeName = $mdgriffith$elm_codegen$Internal$Compiler$denode(myType.ag);
            return details.iG ? addToExposedCollection($stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose({
              ag: typeName,
              ja: $elm$core$Maybe$Just($stil4m$elm_syntax$Elm$Syntax$Range$emptyRange)
            })) : addToExposedCollection($stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose(typeName));
          case 3:
            var myPort = declaration.a;
            var typeName = $mdgriffith$elm_codegen$Internal$Compiler$denode(myPort.ag);
            return addToExposedCollection($stil4m$elm_syntax$Elm$Syntax$Exposing$FunctionExpose(typeName));
          case 4:
            return otherExposes;
          default:
            return otherExposes;
        }
      }
    });
    var $mdgriffith$elm_codegen$Internal$Render$renderDecls = F3(function(fileDetails, decl, gathered) {
      switch (decl.$) {
        case 1:
          var comm = decl.a;
          return _Utils_update(gathered, {
            ix: A2($elm$core$List$cons, $mdgriffith$elm_codegen$Internal$Compiler$RenderedComment(comm), gathered.ix)
          });
        case 3:
          var block = decl.a;
          return _Utils_update(gathered, {
            ix: A2($elm$core$List$cons, $mdgriffith$elm_codegen$Internal$Compiler$RenderedBlock(block), gathered.ix)
          });
        case 2:
          return gathered;
        case 0:
          var decDetails = decl.a;
          var result = decDetails._(fileDetails.at);
          return {
            ix: A2($elm$core$List$cons, $mdgriffith$elm_codegen$Internal$Compiler$RenderedDecl(A2($mdgriffith$elm_codegen$Internal$Render$addDocs, decDetails.aQ, result.iw)), gathered.ix),
            I: gathered.I,
            du: A4($mdgriffith$elm_codegen$Internal$Render$addExposed, gathered.I, decDetails.du, result.iw, gathered.du),
            ad: function() {
              if (gathered.ad) {
                return gathered.ad;
              } else {
                var _v1 = result.iw;
                if (_v1.$ === 3) {
                  return true;
                } else {
                  return false;
                }
              }
            }(),
            c: _Utils_ap(result.X, _Utils_ap(decDetails.c, gathered.c)),
            h0: function() {
              var _v2 = result.jE;
              if (_v2.$ === 1) {
                return gathered.h0;
              } else {
                var warn = _v2.a;
                return A2($elm$core$List$cons, warn, gathered.h0);
              }
            }()
          };
        default:
          var groupDecls = decl.a;
          var incrementExposePath = function(g) {
            return _Utils_update(g, {
              I: function() {
                var _v3 = g.I;
                if (!_v3.b) {
                  return _List_Nil;
                } else {
                  var top = _v3.a;
                  var remain = _v3.b;
                  return A2($elm$core$List$cons, top + 1, remain);
                }
              }()
            });
          };
          return incrementExposePath(A3($elm$core$List$foldl, $mdgriffith$elm_codegen$Internal$Render$renderDecls(fileDetails), _Utils_update(gathered, {
            I: A2($elm$core$List$cons, 0, gathered.I)
          }), groupDecls));
      }
    });
    var $elm$core$String$trim = _String_trim;
    var $the_sett$elm_pretty_printer$Internals$Concatenate = F2(function(a, b) {
      return { $: 1, a, b };
    });
    var $the_sett$elm_pretty_printer$Pretty$append = F2(function(doc1, doc2) {
      return A2($the_sett$elm_pretty_printer$Internals$Concatenate, function(_v0) {
        return doc1;
      }, function(_v1) {
        return doc2;
      });
    });
    var $elm_community$basics_extra$Basics$Extra$flip = F3(function(f, b, a) {
      return A2(f, a, b);
    });
    var $the_sett$elm_pretty_printer$Pretty$a = $elm_community$basics_extra$Basics$Extra$flip($the_sett$elm_pretty_printer$Pretty$append);
    var $the_sett$elm_pretty_printer$Internals$Line = F2(function(a, b) {
      return { $: 4, a, b };
    });
    var $the_sett$elm_pretty_printer$Pretty$line = A2($the_sett$elm_pretty_printer$Internals$Line, " ", "");
    var $the_sett$elm_pretty_printer$Internals$Empty = { $: 0 };
    var $the_sett$elm_pretty_printer$Pretty$empty = $the_sett$elm_pretty_printer$Internals$Empty;
    var $the_sett$elm_pretty_printer$Pretty$join = F2(function(sep, docs) {
      join:
        while (true) {
          if (!docs.b) {
            return $the_sett$elm_pretty_printer$Pretty$empty;
          } else {
            if (!docs.a.$) {
              var _v1 = docs.a;
              var ds = docs.b;
              var $temp$sep = sep, $temp$docs = ds;
              sep = $temp$sep;
              docs = $temp$docs;
              continue join;
            } else {
              var d = docs.a;
              var ds = docs.b;
              var step = F2(function(x, rest) {
                if (!x.$) {
                  return rest;
                } else {
                  var doc = x;
                  return A2($the_sett$elm_pretty_printer$Pretty$append, sep, A2($the_sett$elm_pretty_printer$Pretty$append, doc, rest));
                }
              });
              var spersed = A3($elm$core$List$foldr, step, $the_sett$elm_pretty_printer$Pretty$empty, ds);
              return A2($the_sett$elm_pretty_printer$Pretty$append, d, spersed);
            }
          }
        }
    });
    var $the_sett$elm_pretty_printer$Pretty$lines = $the_sett$elm_pretty_printer$Pretty$join($the_sett$elm_pretty_printer$Pretty$line);
    var $elm$core$Maybe$map = F2(function(f, maybe) {
      if (!maybe.$) {
        var value = maybe.a;
        return $elm$core$Maybe$Just(f(value));
      } else {
        return $elm$core$Maybe$Nothing;
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe = $elm$core$Maybe$map($mdgriffith$elm_codegen$Internal$Compiler$denode);
    var $mdgriffith$elm_codegen$Internal$Compiler$denodeAll = $elm$core$List$map($mdgriffith$elm_codegen$Internal$Compiler$denode);
    var $the_sett$elm_pretty_printer$Internals$Text = F2(function(a, b) {
      return { $: 3, a, b };
    });
    var $elm$core$String$cons = _String_cons;
    var $elm$core$String$fromChar = function(_char) {
      return A2($elm$core$String$cons, _char, "");
    };
    var $the_sett$elm_pretty_printer$Pretty$char = function(c) {
      return A2($the_sett$elm_pretty_printer$Internals$Text, $elm$core$String$fromChar(c), $elm$core$Maybe$Nothing);
    };
    var $the_sett$elm_pretty_printer$Pretty$surround = F3(function(left, right, doc) {
      return A2($the_sett$elm_pretty_printer$Pretty$append, A2($the_sett$elm_pretty_printer$Pretty$append, left, doc), right);
    });
    var $the_sett$elm_pretty_printer$Pretty$parens = function(doc) {
      return A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$char("("), $the_sett$elm_pretty_printer$Pretty$char(")"), doc);
    };
    var $the_sett$elm_pretty_printer$Pretty$string = function(val) {
      return A2($the_sett$elm_pretty_printer$Internals$Text, val, $elm$core$Maybe$Nothing);
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyTopLevelExpose = function(tlExpose) {
      switch (tlExpose.$) {
        case 0:
          var val = tlExpose.a;
          return $the_sett$elm_pretty_printer$Pretty$parens($the_sett$elm_pretty_printer$Pretty$string(val));
        case 1:
          var val = tlExpose.a;
          return $the_sett$elm_pretty_printer$Pretty$string(val);
        case 2:
          var val = tlExpose.a;
          return $the_sett$elm_pretty_printer$Pretty$string(val);
        default:
          var exposedType = tlExpose.a;
          var _v1 = exposedType.ja;
          if (_v1.$ === 1) {
            return $the_sett$elm_pretty_printer$Pretty$string(exposedType.ag);
          } else {
            return A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string("(..)"), $the_sett$elm_pretty_printer$Pretty$string(exposedType.ag));
          }
      }
    };
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$combineTopLevelExposes = function(exposes) {
      if (!exposes.b) {
        return $stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose("");
      } else {
        var hd = exposes.a;
        var tl = exposes.b;
        return A3($elm$core$List$foldl, F2(function(exp, result) {
          var _v1 = _Utils_Tuple2(exp, result);
          if (_v1.a.$ === 3) {
            var typeExpose = _v1.a.a;
            var _v2 = typeExpose.ja;
            if (!_v2.$) {
              return exp;
            } else {
              return result;
            }
          } else {
            if (_v1.b.$ === 3) {
              var typeExpose = _v1.b.a;
              var _v3 = typeExpose.ja;
              if (!_v3.$) {
                return result;
              } else {
                return exp;
              }
            } else {
              return result;
            }
          }
        }), hd, tl);
      }
    };
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName = function(tle) {
      switch (tle.$) {
        case 0:
          var val = tle.a;
          return val;
        case 1:
          var val = tle.a;
          return val;
        case 2:
          var val = tle.a;
          return val;
        default:
          var exposedType = tle.a;
          return exposedType.ag;
      }
    };
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$groupByExposingName = function(innerImports) {
      var _v0 = function() {
        if (!innerImports.b) {
          return _Utils_Tuple3("", _List_Nil, _List_fromArray([_List_Nil]));
        } else {
          var hd = innerImports.a;
          return A3($elm$core$List$foldl, F2(function(exp, _v2) {
            var currName = _v2.a;
            var currAccum = _v2.b;
            var accum = _v2.c;
            var nextName = $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(exp);
            return _Utils_eq(nextName, currName) ? _Utils_Tuple3(currName, A2($elm$core$List$cons, exp, currAccum), accum) : _Utils_Tuple3(nextName, _List_fromArray([exp]), A2($elm$core$List$cons, currAccum, accum));
          }), _Utils_Tuple3($mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(hd), _List_Nil, _List_Nil), innerImports);
        }
      }();
      var hdGroup = _v0.b;
      var remGroups = _v0.c;
      return $elm$core$List$reverse(A2($elm$core$List$cons, hdGroup, remGroups));
    };
    var $elm$core$List$sortWith = _List_sortWith;
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeOrder = F2(function(tlel, tler) {
      var _v0 = _Utils_Tuple2(tlel, tler);
      if (!_v0.a.$) {
        if (!_v0.b.$) {
          return A2($elm$core$Basics$compare, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(tlel), $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(tler));
        } else {
          return 0;
        }
      } else {
        if (!_v0.b.$) {
          return 2;
        } else {
          return A2($elm$core$Basics$compare, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(tlel), $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(tler));
        }
      }
    });
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupExposings = function(tlExposings) {
      return A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$combineTopLevelExposes, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$groupByExposingName(A2($elm$core$List$sortWith, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeOrder, tlExposings)));
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyExposing = function(exposing_) {
      var exposings = function() {
        if (!exposing_.$) {
          return A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$string(" ("), $the_sett$elm_pretty_printer$Pretty$string(")"), $the_sett$elm_pretty_printer$Pretty$string(".."));
        } else {
          var tll = exposing_.a;
          return $elm$core$List$length(tll) <= 5 ? A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$string(" ("), $the_sett$elm_pretty_printer$Pretty$string(")"), A2($the_sett$elm_pretty_printer$Pretty$join, $the_sett$elm_pretty_printer$Pretty$string(", "), A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettyTopLevelExpose, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupExposings($mdgriffith$elm_codegen$Internal$Compiler$denodeAll(tll))))) : A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$string(`
    ( `), $the_sett$elm_pretty_printer$Pretty$string(`
    )`), A2($the_sett$elm_pretty_printer$Pretty$join, $the_sett$elm_pretty_printer$Pretty$string(`
    , `), A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettyTopLevelExpose, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupExposings($mdgriffith$elm_codegen$Internal$Compiler$denodeAll(tll)))));
        }
      }();
      return A2($the_sett$elm_pretty_printer$Pretty$a, exposings, $the_sett$elm_pretty_printer$Pretty$string("exposing"));
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyMaybe = F2(function(prettyFn, maybeVal) {
      return A2($elm$core$Maybe$withDefault, $the_sett$elm_pretty_printer$Pretty$empty, A2($elm$core$Maybe$map, prettyFn, maybeVal));
    });
    var $mdgriffith$elm_codegen$Internal$Write$dot = $the_sett$elm_pretty_printer$Pretty$string(".");
    var $mdgriffith$elm_codegen$Internal$Write$prettyModuleName = function(name) {
      return A2($the_sett$elm_pretty_printer$Pretty$join, $mdgriffith$elm_codegen$Internal$Write$dot, A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, name));
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyModuleNameAlias = function(name) {
      if (!name.b) {
        return $the_sett$elm_pretty_printer$Pretty$empty;
      } else {
        return A2($the_sett$elm_pretty_printer$Pretty$a, A2($the_sett$elm_pretty_printer$Pretty$join, $mdgriffith$elm_codegen$Internal$Write$dot, A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, name)), $the_sett$elm_pretty_printer$Pretty$string("as "));
      }
    };
    var $the_sett$elm_pretty_printer$Pretty$space = $the_sett$elm_pretty_printer$Pretty$char(" ");
    var $mdgriffith$elm_codegen$Internal$Write$prettyImport = function(import_) {
      return A2($the_sett$elm_pretty_printer$Pretty$join, $the_sett$elm_pretty_printer$Pretty$space, _List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string("import"),
        $mdgriffith$elm_codegen$Internal$Write$prettyModuleName($mdgriffith$elm_codegen$Internal$Compiler$denode(import_.a3)),
        A2($mdgriffith$elm_codegen$Internal$Write$prettyMaybe, $mdgriffith$elm_codegen$Internal$Write$prettyModuleNameAlias, $mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(import_.fi)),
        A2($mdgriffith$elm_codegen$Internal$Write$prettyMaybe, $mdgriffith$elm_codegen$Internal$Write$prettyExposing, $mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(import_.dv))
      ]));
    };
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode = $stil4m$elm_syntax$Elm$Syntax$Node$value;
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeMaybe = $elm$core$Maybe$map($mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode);
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeAll = $elm$core$List$map($mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode);
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodify = function(exp) {
      return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, exp);
    };
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodifyAll = $elm$core$List$map($mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodify);
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$joinExposings = F2(function(left, right) {
      var _v0 = _Utils_Tuple2(left, right);
      if (!_v0.a.$) {
        var range = _v0.a.a;
        return $stil4m$elm_syntax$Elm$Syntax$Exposing$All(range);
      } else {
        if (!_v0.b.$) {
          var range = _v0.b.a;
          return $stil4m$elm_syntax$Elm$Syntax$Exposing$All(range);
        } else {
          var leftNodes = _v0.a.a;
          var rightNodes = _v0.b.a;
          return $stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit($mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodifyAll(A2($elm$core$List$append, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeAll(leftNodes), $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeAll(rightNodes))));
        }
      }
    });
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$joinMaybeExposings = F2(function(maybeLeft, maybeRight) {
      var _v0 = _Utils_Tuple2(maybeLeft, maybeRight);
      if (_v0.a.$ === 1) {
        if (_v0.b.$ === 1) {
          var _v1 = _v0.a;
          var _v2 = _v0.b;
          return $elm$core$Maybe$Nothing;
        } else {
          var _v4 = _v0.a;
          var right = _v0.b.a;
          return $elm$core$Maybe$Just(right);
        }
      } else {
        if (_v0.b.$ === 1) {
          var left = _v0.a.a;
          var _v3 = _v0.b;
          return $elm$core$Maybe$Just(left);
        } else {
          var left = _v0.a.a;
          var right = _v0.b.a;
          return $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Internal$ImportsAndExposing$joinExposings, left, right));
        }
      }
    });
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodifyMaybe = $elm$core$Maybe$map($mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodify);
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$or = F2(function(ma, mb) {
      if (ma.$ === 1) {
        return mb;
      } else {
        return ma;
      }
    });
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupExposing = function(exp) {
      if (!exp.$) {
        var range = exp.a;
        return $stil4m$elm_syntax$Elm$Syntax$Exposing$All(range);
      } else {
        var nodes = exp.a;
        return $stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit($mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodifyAll($mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupExposings($mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeAll(nodes))));
      }
    };
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$combineImports = function(innerImports) {
      if (!innerImports.b) {
        return {
          dv: $elm$core$Maybe$Nothing,
          fi: $elm$core$Maybe$Nothing,
          a3: $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodify(_List_Nil)
        };
      } else {
        var hd = innerImports.a;
        var tl = innerImports.b;
        var combinedImports = A3($elm$core$List$foldl, F2(function(imp, result) {
          return {
            dv: $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodifyMaybe(A2($mdgriffith$elm_codegen$Internal$ImportsAndExposing$joinMaybeExposings, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeMaybe(imp.dv), $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeMaybe(result.dv))),
            fi: A2($mdgriffith$elm_codegen$Internal$ImportsAndExposing$or, imp.fi, result.fi),
            a3: imp.a3
          };
        }), hd, tl);
        return _Utils_update(combinedImports, {
          dv: A2($elm$core$Maybe$map, A2($elm$core$Basics$composeR, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode, A2($elm$core$Basics$composeR, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupExposing, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodify)), combinedImports.dv)
        });
      }
    };
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$groupByModuleName = function(innerImports) {
      var _v0 = function() {
        if (!innerImports.b) {
          return _Utils_Tuple3(_List_Nil, _List_Nil, _List_fromArray([_List_Nil]));
        } else {
          var hd = innerImports.a;
          return A3($elm$core$List$foldl, F2(function(imp, _v2) {
            var currName = _v2.a;
            var currAccum = _v2.b;
            var accum = _v2.c;
            var nextName = $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode(imp.a3);
            return _Utils_eq(nextName, currName) ? _Utils_Tuple3(currName, A2($elm$core$List$cons, imp, currAccum), accum) : _Utils_Tuple3(nextName, _List_fromArray([imp]), A2($elm$core$List$cons, currAccum, accum));
          }), _Utils_Tuple3($mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode(hd.a3), _List_Nil, _List_Nil), innerImports);
        }
      }();
      var hdGroup = _v0.b;
      var remGroups = _v0.c;
      return $elm$core$List$reverse(A2($elm$core$List$cons, hdGroup, remGroups));
    };
    var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupImports = function(imports) {
      var impName = function(imp) {
        return $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode(imp.a3);
      };
      return A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$combineImports, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$groupByModuleName(A2($elm$core$List$sortBy, impName, imports)));
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyImports = function(imports) {
      return $the_sett$elm_pretty_printer$Pretty$lines(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettyImport, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupImports(imports)));
    };
    var $mdgriffith$elm_codegen$Internal$Write$importsPretty = function(imports) {
      if (!imports.b) {
        return $the_sett$elm_pretty_printer$Pretty$line;
      } else {
        return A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, $mdgriffith$elm_codegen$Internal$Write$prettyImports(imports))));
      }
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyComments = function(comments) {
      if (!comments.b) {
        return $the_sett$elm_pretty_printer$Pretty$empty;
      } else {
        return A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, $the_sett$elm_pretty_printer$Pretty$lines(A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, comments))));
      }
    };
    var $elm$core$List$drop = F2(function(n, list) {
      drop:
        while (true) {
          if (n <= 0) {
            return list;
          } else {
            if (!list.b) {
              return list;
            } else {
              var x = list.a;
              var xs = list.b;
              var $temp$n = n - 1, $temp$list = xs;
              n = $temp$n;
              list = $temp$list;
              continue drop;
            }
          }
        }
    });
    var $the_sett$elm_pretty_printer$Internals$Nest = F2(function(a, b) {
      return { $: 2, a, b };
    });
    var $the_sett$elm_pretty_printer$Pretty$nest = F2(function(depth, doc) {
      return A2($the_sett$elm_pretty_printer$Internals$Nest, depth, function(_v0) {
        return doc;
      });
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyDocumentation = function(docs) {
      return A2($elm$core$String$contains, `
`, docs) ? $the_sett$elm_pretty_printer$Pretty$string("{-| " + (docs + `
-}`)) : $the_sett$elm_pretty_printer$Pretty$string("{-| " + (docs + " -}"));
    };
    var $the_sett$elm_pretty_printer$Internals$Union = F2(function(a, b) {
      return { $: 5, a, b };
    });
    var $the_sett$elm_pretty_printer$Internals$flatten = function(doc) {
      flatten:
        while (true) {
          switch (doc.$) {
            case 1:
              var doc1 = doc.a;
              var doc2 = doc.b;
              return A2($the_sett$elm_pretty_printer$Internals$Concatenate, function(_v1) {
                return $the_sett$elm_pretty_printer$Internals$flatten(doc1(0));
              }, function(_v2) {
                return $the_sett$elm_pretty_printer$Internals$flatten(doc2(0));
              });
            case 2:
              var i = doc.a;
              var doc1 = doc.b;
              return A2($the_sett$elm_pretty_printer$Internals$Nest, i, function(_v3) {
                return $the_sett$elm_pretty_printer$Internals$flatten(doc1(0));
              });
            case 5:
              var doc1 = doc.a;
              var doc2 = doc.b;
              var $temp$doc = doc1;
              doc = $temp$doc;
              continue flatten;
            case 4:
              var hsep = doc.a;
              return A2($the_sett$elm_pretty_printer$Internals$Text, hsep, $elm$core$Maybe$Nothing);
            case 6:
              var fn = doc.a;
              var $temp$doc = fn(0);
              doc = $temp$doc;
              continue flatten;
            case 7:
              var fn = doc.a;
              var $temp$doc = fn(0);
              doc = $temp$doc;
              continue flatten;
            default:
              var x = doc;
              return x;
          }
        }
    };
    var $the_sett$elm_pretty_printer$Pretty$group = function(doc) {
      return A2($the_sett$elm_pretty_printer$Internals$Union, $the_sett$elm_pretty_printer$Internals$flatten(doc), doc);
    };
    var $mdgriffith$elm_codegen$Internal$Write$isNakedCompound = function(typeAnn) {
      switch (typeAnn.$) {
        case 1:
          if (!typeAnn.b.b) {
            return false;
          } else {
            return true;
          }
        case 6:
          return true;
        default:
          return false;
      }
    };
    var $elm$core$Tuple$mapBoth = F3(function(funcA, funcB, _v0) {
      var x = _v0.a;
      var y = _v0.b;
      return _Utils_Tuple2(funcA(x), funcB(y));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyModuleNameDot = F2(function(aliases, name) {
      if (!name.b) {
        return $the_sett$elm_pretty_printer$Pretty$empty;
      } else {
        var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$findAlias, name, aliases);
        if (_v1.$ === 1) {
          return A2($the_sett$elm_pretty_printer$Pretty$a, $mdgriffith$elm_codegen$Internal$Write$dot, A2($the_sett$elm_pretty_printer$Pretty$join, $mdgriffith$elm_codegen$Internal$Write$dot, A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, name)));
        } else {
          var alias = _v1.a;
          return A2($the_sett$elm_pretty_printer$Pretty$a, $mdgriffith$elm_codegen$Internal$Write$dot, $the_sett$elm_pretty_printer$Pretty$string(alias));
        }
      }
    });
    var $the_sett$elm_pretty_printer$Pretty$separators = function(sep) {
      return $the_sett$elm_pretty_printer$Pretty$join(A2($the_sett$elm_pretty_printer$Internals$Line, sep, sep));
    };
    var $the_sett$elm_pretty_printer$Pretty$words = $the_sett$elm_pretty_printer$Pretty$join($the_sett$elm_pretty_printer$Pretty$space);
    var $mdgriffith$elm_codegen$Internal$Write$prettyFieldTypeAnn = F2(function(aliases, _v13) {
      var name = _v13.a;
      var ann = _v13.b;
      return $the_sett$elm_pretty_printer$Pretty$group(A2($the_sett$elm_pretty_printer$Pretty$nest, 4, $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
          $the_sett$elm_pretty_printer$Pretty$string(name),
          $the_sett$elm_pretty_printer$Pretty$string(":")
        ])),
        A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, ann)
      ]))));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyFunctionTypeAnnotation = F3(function(aliases, left, right) {
      var expandLeft = function(ann) {
        if (ann.$ === 6) {
          return A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotationParens, aliases, ann);
        } else {
          return A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, ann);
        }
      };
      var innerFnTypeAnn = F2(function(_v10, _v11) {
        var innerLeft = _v10.b;
        var innerRight = _v11.b;
        var rightSide = expandRight(innerRight);
        if (rightSide.b) {
          var hd = rightSide.a;
          var tl = rightSide.b;
          return A2($elm$core$List$cons, expandLeft(innerLeft), A2($elm$core$List$cons, $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
            $the_sett$elm_pretty_printer$Pretty$string("->"),
            hd
          ])), tl));
        } else {
          return _List_Nil;
        }
      });
      var expandRight = function(ann) {
        if (ann.$ === 6) {
          var innerLeft = ann.a;
          var innerRight = ann.b;
          return A2(innerFnTypeAnn, innerLeft, innerRight);
        } else {
          return _List_fromArray([
            A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, ann)
          ]);
        }
      };
      return $the_sett$elm_pretty_printer$Pretty$group($the_sett$elm_pretty_printer$Pretty$lines(A2(innerFnTypeAnn, left, right)));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyGenericRecord = F3(function(aliases, paramName, fields) {
      var addBarToFirst = function(exprs) {
        if (!exprs.b) {
          return _List_Nil;
        } else {
          var hd = exprs.a;
          var tl = exprs.b;
          return A2($elm$core$List$cons, A2($the_sett$elm_pretty_printer$Pretty$a, hd, $the_sett$elm_pretty_printer$Pretty$string("| ")), tl);
        }
      };
      if (!fields.b) {
        return $the_sett$elm_pretty_printer$Pretty$string("{}");
      } else {
        var open = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
          $the_sett$elm_pretty_printer$Pretty$string("{"),
          $the_sett$elm_pretty_printer$Pretty$string(paramName)
        ])));
        var close = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string("}"), $the_sett$elm_pretty_printer$Pretty$line);
        return $the_sett$elm_pretty_printer$Pretty$group(A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$empty, close, A2($the_sett$elm_pretty_printer$Pretty$nest, 4, A2($the_sett$elm_pretty_printer$Pretty$a, A2($the_sett$elm_pretty_printer$Pretty$separators, ", ", addBarToFirst(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettyFieldTypeAnn(aliases), A2($elm$core$List$map, A2($elm$core$Tuple$mapBoth, $mdgriffith$elm_codegen$Internal$Compiler$denode, $mdgriffith$elm_codegen$Internal$Compiler$denode), fields)))), open))));
      }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyRecord = F2(function(aliases, fields) {
      if (!fields.b) {
        return $the_sett$elm_pretty_printer$Pretty$string("{}");
      } else {
        var open = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$space, $the_sett$elm_pretty_printer$Pretty$string("{"));
        var close = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string("}"), $the_sett$elm_pretty_printer$Pretty$line);
        return $the_sett$elm_pretty_printer$Pretty$group(A3($the_sett$elm_pretty_printer$Pretty$surround, open, close, A2($the_sett$elm_pretty_printer$Pretty$separators, ", ", A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettyFieldTypeAnn(aliases), A2($elm$core$List$map, A2($elm$core$Tuple$mapBoth, $mdgriffith$elm_codegen$Internal$Compiler$denode, $mdgriffith$elm_codegen$Internal$Compiler$denode), fields)))));
      }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyTupled = F2(function(aliases, anns) {
      return $the_sett$elm_pretty_printer$Pretty$parens(A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$space, A2($the_sett$elm_pretty_printer$Pretty$a, A2($the_sett$elm_pretty_printer$Pretty$join, $the_sett$elm_pretty_printer$Pretty$string(", "), A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation(aliases), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(anns))), $the_sett$elm_pretty_printer$Pretty$space)));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation = F2(function(aliases, typeAnn) {
      switch (typeAnn.$) {
        case 0:
          var val = typeAnn.a;
          return $the_sett$elm_pretty_printer$Pretty$string(val);
        case 1:
          var fqName = typeAnn.a;
          var anns = typeAnn.b;
          return A3($mdgriffith$elm_codegen$Internal$Write$prettyTyped, aliases, fqName, anns);
        case 2:
          return $the_sett$elm_pretty_printer$Pretty$string("()");
        case 3:
          var anns = typeAnn.a;
          return A2($mdgriffith$elm_codegen$Internal$Write$prettyTupled, aliases, anns);
        case 4:
          var recordDef = typeAnn.a;
          return A2($mdgriffith$elm_codegen$Internal$Write$prettyRecord, aliases, $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(recordDef));
        case 5:
          var _v3 = typeAnn.a;
          var paramName = _v3.b;
          var _v4 = typeAnn.b;
          var recordDef = _v4.b;
          return A3($mdgriffith$elm_codegen$Internal$Write$prettyGenericRecord, aliases, paramName, $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(recordDef));
        default:
          var fromAnn = typeAnn.a;
          var toAnn = typeAnn.b;
          return A3($mdgriffith$elm_codegen$Internal$Write$prettyFunctionTypeAnnotation, aliases, fromAnn, toAnn);
      }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotationParens = F2(function(aliases, typeAnn) {
      return $mdgriffith$elm_codegen$Internal$Write$isNakedCompound(typeAnn) ? $the_sett$elm_pretty_printer$Pretty$parens(A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, typeAnn)) : A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, typeAnn);
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyTyped = F3(function(aliases, _v0, anns) {
      var _v1 = _v0.b;
      var moduleName = _v1.a;
      var typeName = _v1.b;
      var typeDoc = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string(typeName), A2($mdgriffith$elm_codegen$Internal$Write$prettyModuleNameDot, aliases, moduleName));
      var argsDoc = $the_sett$elm_pretty_printer$Pretty$words(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotationParens(aliases), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(anns)));
      return $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([typeDoc, argsDoc]));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyValueConstructor = F2(function(aliases, cons) {
      return A2($the_sett$elm_pretty_printer$Pretty$nest, 4, $the_sett$elm_pretty_printer$Pretty$group($the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string($mdgriffith$elm_codegen$Internal$Compiler$denode(cons.ag)),
        $the_sett$elm_pretty_printer$Pretty$lines(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotationParens(aliases), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(cons.aL)))
      ]))));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyValueConstructors = F2(function(aliases, constructors) {
      return A2($the_sett$elm_pretty_printer$Pretty$join, A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string("| "), $the_sett$elm_pretty_printer$Pretty$line), A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettyValueConstructor(aliases), constructors));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyCustomType = F2(function(aliases, type_) {
      var customTypePretty = A2($the_sett$elm_pretty_printer$Pretty$nest, 4, A2($the_sett$elm_pretty_printer$Pretty$a, A2($mdgriffith$elm_codegen$Internal$Write$prettyValueConstructors, aliases, $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(type_.it)), A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string("= "), A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string("type"),
        $the_sett$elm_pretty_printer$Pretty$string($mdgriffith$elm_codegen$Internal$Compiler$denode(type_.ag)),
        $the_sett$elm_pretty_printer$Pretty$words(A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(type_.dZ)))
      ]))))));
      return $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
        A2($mdgriffith$elm_codegen$Internal$Write$prettyMaybe, $mdgriffith$elm_codegen$Internal$Write$prettyDocumentation, $mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(type_.aR)),
        customTypePretty
      ]));
    });
    var $the_sett$elm_pretty_printer$Internals$Column = function(a) {
      return { $: 7, a };
    };
    var $the_sett$elm_pretty_printer$Pretty$column = $the_sett$elm_pretty_printer$Internals$Column;
    var $the_sett$elm_pretty_printer$Internals$Nesting = function(a) {
      return { $: 6, a };
    };
    var $the_sett$elm_pretty_printer$Pretty$nesting = $the_sett$elm_pretty_printer$Internals$Nesting;
    var $the_sett$elm_pretty_printer$Pretty$align = function(doc) {
      return $the_sett$elm_pretty_printer$Pretty$column(function(currentColumn) {
        return $the_sett$elm_pretty_printer$Pretty$nesting(function(indentLvl) {
          return A2($the_sett$elm_pretty_printer$Pretty$nest, currentColumn - indentLvl, doc);
        });
      });
    };
    var $mdgriffith$elm_codegen$Internal$Write$bottomContext = { je: 11 };
    var $elm$core$Basics$modBy = _Basics_modBy;
    var $mdgriffith$elm_codegen$Internal$Write$decrementIndent = F2(function(currentIndent, spaces) {
      var modded = A2($elm$core$Basics$modBy, 4, currentIndent - spaces);
      return !modded ? 4 : modded;
    });
    var $mdgriffith$elm_codegen$Internal$Write$doubleLines = $the_sett$elm_pretty_printer$Pretty$join(A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, $the_sett$elm_pretty_printer$Pretty$line));
    var $mdgriffith$elm_codegen$Internal$Write$escapeChar = function(val) {
      switch (val) {
        case "\\":
          return "\\\\";
        case "'":
          return "\\'";
        case "\t":
          return "\\t";
        case `
`:
          return "\\n";
        default:
          var c = val;
          return $elm$core$String$fromChar(c);
      }
    };
    var $elm$core$String$fromFloat = _String_fromNumber;
    var $the_sett$elm_pretty_printer$Internals$copy = F2(function(i, s) {
      return !i ? "" : _Utils_ap(s, A2($the_sett$elm_pretty_printer$Internals$copy, i - 1, s));
    });
    var $the_sett$elm_pretty_printer$Pretty$hang = F2(function(spaces, doc) {
      return $the_sett$elm_pretty_printer$Pretty$align(A2($the_sett$elm_pretty_printer$Pretty$nest, spaces, doc));
    });
    var $the_sett$elm_pretty_printer$Pretty$indent = F2(function(spaces, doc) {
      return A2($the_sett$elm_pretty_printer$Pretty$hang, spaces, A2($the_sett$elm_pretty_printer$Pretty$append, $the_sett$elm_pretty_printer$Pretty$string(A2($the_sett$elm_pretty_printer$Internals$copy, spaces, " ")), doc));
    });
    var $mdgriffith$elm_codegen$Internal$Write$optionalGroup = F2(function(flag, doc) {
      return flag ? doc : $the_sett$elm_pretty_printer$Pretty$group(doc);
    });
    var $mdgriffith$elm_codegen$Internal$Write$precedence = function(symbol) {
      switch (symbol) {
        case ">>":
          return 9;
        case "<<":
          return 9;
        case "^":
          return 8;
        case "*":
          return 7;
        case "/":
          return 7;
        case "//":
          return 7;
        case "%":
          return 7;
        case "rem":
          return 7;
        case "+":
          return 6;
        case "-":
          return 6;
        case "++":
          return 5;
        case "::":
          return 5;
        case "==":
          return 4;
        case "/=":
          return 4;
        case "<":
          return 4;
        case ">":
          return 4;
        case "<=":
          return 4;
        case ">=":
          return 4;
        case "&&":
          return 3;
        case "||":
          return 2;
        case "|>":
          return 0;
        case "<|":
          return 0;
        default:
          return 0;
      }
    };
    var $stil4m$elm_syntax$Elm$Syntax$Pattern$ParenthesizedPattern = function(a) {
      return { $: 14, a };
    };
    var $mdgriffith$elm_codegen$Internal$Write$adjustPatternParentheses = F2(function(isTop, pattern) {
      var shouldRemove = function(pat) {
        var _v6 = _Utils_Tuple2(isTop, pat);
        _v6$2:
          while (true) {
            switch (_v6.b.$) {
              case 12:
                if (!_v6.a) {
                  var _v7 = _v6.b;
                  return false;
                } else {
                  break _v6$2;
                }
              case 13:
                var _v8 = _v6.b;
                return false;
              default:
                break _v6$2;
            }
          }
        return isTop;
      };
      var removeParens = function(pat) {
        removeParens:
          while (true) {
            if (pat.$ === 14) {
              var _v1 = pat.a;
              var innerPat = _v1.b;
              if (shouldRemove(innerPat)) {
                var $temp$pat = innerPat;
                pat = $temp$pat;
                continue removeParens;
              } else {
                return pat;
              }
            } else {
              return pat;
            }
          }
      };
      var addParens = function(pat) {
        var _v2 = _Utils_Tuple2(isTop, pat);
        _v2$2:
          while (true) {
            if (!_v2.a) {
              switch (_v2.b.$) {
                case 12:
                  if (_v2.b.b.b) {
                    var _v3 = _v2.b;
                    var _v4 = _v3.b;
                    return $stil4m$elm_syntax$Elm$Syntax$Pattern$ParenthesizedPattern($mdgriffith$elm_codegen$Internal$Compiler$nodify(pat));
                  } else {
                    break _v2$2;
                  }
                case 13:
                  var _v5 = _v2.b;
                  return $stil4m$elm_syntax$Elm$Syntax$Pattern$ParenthesizedPattern($mdgriffith$elm_codegen$Internal$Compiler$nodify(pat));
                default:
                  break _v2$2;
              }
            } else {
              break _v2$2;
            }
          }
        return pat;
      };
      return addParens(removeParens(pattern));
    });
    var $the_sett$elm_pretty_printer$Pretty$braces = function(doc) {
      return A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$char("{"), $the_sett$elm_pretty_printer$Pretty$char("}"), doc);
    };
    var $mdgriffith$elm_codegen$Internal$Write$quotes = function(doc) {
      return A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$char('"'), $the_sett$elm_pretty_printer$Pretty$char('"'), doc);
    };
    var $mdgriffith$elm_codegen$Internal$Write$singleQuotes = function(doc) {
      return A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$char("'"), $the_sett$elm_pretty_printer$Pretty$char("'"), doc);
    };
    var $elm$core$String$fromList = _String_fromList;
    var $rtfeldman$elm_hex$Hex$unsafeToDigit = function(num) {
      unsafeToDigit:
        while (true) {
          switch (num) {
            case 0:
              return "0";
            case 1:
              return "1";
            case 2:
              return "2";
            case 3:
              return "3";
            case 4:
              return "4";
            case 5:
              return "5";
            case 6:
              return "6";
            case 7:
              return "7";
            case 8:
              return "8";
            case 9:
              return "9";
            case 10:
              return "a";
            case 11:
              return "b";
            case 12:
              return "c";
            case 13:
              return "d";
            case 14:
              return "e";
            case 15:
              return "f";
            default:
              var $temp$num = num;
              num = $temp$num;
              continue unsafeToDigit;
          }
        }
    };
    var $rtfeldman$elm_hex$Hex$unsafePositiveToDigits = F2(function(digits, num) {
      unsafePositiveToDigits:
        while (true) {
          if (num < 16) {
            return A2($elm$core$List$cons, $rtfeldman$elm_hex$Hex$unsafeToDigit(num), digits);
          } else {
            var $temp$digits = A2($elm$core$List$cons, $rtfeldman$elm_hex$Hex$unsafeToDigit(A2($elm$core$Basics$modBy, 16, num)), digits), $temp$num = num / 16 | 0;
            digits = $temp$digits;
            num = $temp$num;
            continue unsafePositiveToDigits;
          }
        }
    });
    var $rtfeldman$elm_hex$Hex$toString = function(num) {
      return $elm$core$String$fromList(num < 0 ? A2($elm$core$List$cons, "-", A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, -num)) : A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, num));
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyPatternInner = F3(function(aliases, isTop, pattern) {
      var _v0 = A2($mdgriffith$elm_codegen$Internal$Write$adjustPatternParentheses, isTop, pattern);
      switch (_v0.$) {
        case 0:
          return $the_sett$elm_pretty_printer$Pretty$string("_");
        case 1:
          return $the_sett$elm_pretty_printer$Pretty$string("()");
        case 2:
          var val = _v0.a;
          return $mdgriffith$elm_codegen$Internal$Write$singleQuotes($the_sett$elm_pretty_printer$Pretty$string($mdgriffith$elm_codegen$Internal$Write$escapeChar(val)));
        case 3:
          var val = _v0.a;
          return $mdgriffith$elm_codegen$Internal$Write$quotes($the_sett$elm_pretty_printer$Pretty$string(val));
        case 4:
          var val = _v0.a;
          return $the_sett$elm_pretty_printer$Pretty$string($elm$core$String$fromInt(val));
        case 5:
          var val = _v0.a;
          return $the_sett$elm_pretty_printer$Pretty$string($rtfeldman$elm_hex$Hex$toString(val));
        case 6:
          var val = _v0.a;
          return $the_sett$elm_pretty_printer$Pretty$string($elm$core$String$fromFloat(val));
        case 7:
          var vals = _v0.a;
          return $the_sett$elm_pretty_printer$Pretty$parens(A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$space, A2($the_sett$elm_pretty_printer$Pretty$a, A2($the_sett$elm_pretty_printer$Pretty$join, $the_sett$elm_pretty_printer$Pretty$string(", "), A2($elm$core$List$map, A2($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, true), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(vals))), $the_sett$elm_pretty_printer$Pretty$space)));
        case 8:
          var fields = _v0.a;
          return $the_sett$elm_pretty_printer$Pretty$braces(A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$space, $the_sett$elm_pretty_printer$Pretty$space, A2($the_sett$elm_pretty_printer$Pretty$join, $the_sett$elm_pretty_printer$Pretty$string(", "), A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(fields)))));
        case 9:
          var _v1 = _v0.a;
          var hdPat = _v1.b;
          var _v2 = _v0.b;
          var tlPat = _v2.b;
          return $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
            A3($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false, hdPat),
            $the_sett$elm_pretty_printer$Pretty$string("::"),
            A3($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false, tlPat)
          ]));
        case 10:
          var listPats = _v0.a;
          if (!listPats.b) {
            return $the_sett$elm_pretty_printer$Pretty$string("[]");
          } else {
            var open = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$space, $the_sett$elm_pretty_printer$Pretty$string("["));
            var close = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string("]"), $the_sett$elm_pretty_printer$Pretty$space);
            return A3($the_sett$elm_pretty_printer$Pretty$surround, open, close, A2($the_sett$elm_pretty_printer$Pretty$join, $the_sett$elm_pretty_printer$Pretty$string(", "), A2($elm$core$List$map, A2($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(listPats))));
          }
        case 11:
          var _var = _v0.a;
          return $the_sett$elm_pretty_printer$Pretty$string(_var);
        case 12:
          var qnRef = _v0.a;
          var listPats = _v0.b;
          return $the_sett$elm_pretty_printer$Pretty$words(A2($elm$core$List$cons, A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string(qnRef.ag), A2($mdgriffith$elm_codegen$Internal$Write$prettyModuleNameDot, aliases, qnRef.a3)), A2($elm$core$List$map, A2($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(listPats))));
        case 13:
          var _v4 = _v0.a;
          var pat = _v4.b;
          var _v5 = _v0.b;
          var name = _v5.b;
          return $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
            A3($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false, pat),
            $the_sett$elm_pretty_printer$Pretty$string("as"),
            $the_sett$elm_pretty_printer$Pretty$string(name)
          ]));
        default:
          var _v6 = _v0.a;
          var pat = _v6.b;
          return $the_sett$elm_pretty_printer$Pretty$parens(A3($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, true, pat));
      }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyArgs = F2(function(aliases, args) {
      return $the_sett$elm_pretty_printer$Pretty$words(A2($elm$core$List$map, A2($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false), args));
    });
    var $mdgriffith$elm_codegen$Internal$Write$escape = function(val) {
      return A3($elm$core$String$replace, "\t", "\\t", A3($elm$core$String$replace, `
`, "\\n", A3($elm$core$String$replace, '"', "\\\"", A3($elm$core$String$replace, "\\", "\\\\", val))));
    };
    var $mdgriffith$elm_codegen$Internal$Write$tripleQuotes = function(doc) {
      return A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$string('"""'), $the_sett$elm_pretty_printer$Pretty$string('"""'), doc);
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyLiteral = function(val) {
      return A2($elm$core$String$contains, `
`, val) ? $mdgriffith$elm_codegen$Internal$Write$tripleQuotes($the_sett$elm_pretty_printer$Pretty$string(val)) : $mdgriffith$elm_codegen$Internal$Write$quotes($the_sett$elm_pretty_printer$Pretty$string($mdgriffith$elm_codegen$Internal$Write$escape(val)));
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyPattern = F2(function(aliases, pattern) {
      return A3($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, true, pattern);
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettySignature = F2(function(aliases, sig) {
      return $the_sett$elm_pretty_printer$Pretty$group(A2($the_sett$elm_pretty_printer$Pretty$nest, 4, $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
          $the_sett$elm_pretty_printer$Pretty$string($mdgriffith$elm_codegen$Internal$Compiler$denode(sig.ag)),
          $the_sett$elm_pretty_printer$Pretty$string(":")
        ])),
        A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, $mdgriffith$elm_codegen$Internal$Compiler$denode(sig.aH))
      ]))));
    });
    var $the_sett$elm_pretty_printer$Pretty$tightline = A2($the_sett$elm_pretty_printer$Internals$Line, "", "");
    var $mdgriffith$elm_codegen$Internal$Write$showParen = F2(function(show, _v0) {
      var child = _v0.a;
      var alwaysBreak = _v0.b;
      if (show) {
        var open = $the_sett$elm_pretty_printer$Pretty$string("(");
        var close = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string(")"), $the_sett$elm_pretty_printer$Pretty$tightline);
        return _Utils_Tuple2(A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$align(A3($the_sett$elm_pretty_printer$Pretty$surround, open, close, A2($the_sett$elm_pretty_printer$Pretty$nest, 1, child)))), alwaysBreak);
      } else {
        return _Utils_Tuple2(child, alwaysBreak);
      }
    });
    var $elm$core$String$padLeft = F3(function(n, _char, string) {
      return _Utils_ap(A2($elm$core$String$repeat, n - $elm$core$String$length(string), $elm$core$String$fromChar(_char)), string);
    });
    var $mdgriffith$elm_codegen$Internal$Write$toHexString = function(val) {
      var padWithZeros = function(str) {
        var length = $elm$core$String$length(str);
        return length < 2 ? A3($elm$core$String$padLeft, 2, "0", str) : length > 2 && length < 4 ? A3($elm$core$String$padLeft, 4, "0", str) : length > 4 && length < 8 ? A3($elm$core$String$padLeft, 8, "0", str) : str;
      };
      return "0x" + padWithZeros($elm$core$String$toUpper($rtfeldman$elm_hex$Hex$toString(val)));
    };
    var $mdgriffith$elm_codegen$Internal$Write$topContext = { je: 0 };
    var $elm$core$List$unzip = function(pairs) {
      var step = F2(function(_v0, _v1) {
        var x = _v0.a;
        var y = _v0.b;
        var xs = _v1.a;
        var ys = _v1.b;
        return _Utils_Tuple2(A2($elm$core$List$cons, x, xs), A2($elm$core$List$cons, y, ys));
      });
      return A3($elm$core$List$foldr, step, _Utils_Tuple2(_List_Nil, _List_Nil), pairs);
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyApplication = F3(function(aliases, indent, exprs) {
      var _v41 = A2($elm$core$Tuple$mapSecond, $elm$core$List$any($elm$core$Basics$identity), $elm$core$List$unzip(A2($elm$core$List$map, A3($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$bottomContext, indent), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(exprs))));
      var prettyExpressions = _v41.a;
      var alwaysBreak = _v41.b;
      return _Utils_Tuple2(A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$align(A2($the_sett$elm_pretty_printer$Pretty$nest, indent, $the_sett$elm_pretty_printer$Pretty$lines(prettyExpressions)))), alwaysBreak);
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyCaseBlock = F3(function(aliases, indent, caseBlock) {
      var prettyCase = function(_v38) {
        var _v39 = _v38.a;
        var pattern = _v39.b;
        var _v40 = _v38.b;
        var expr = _v40.b;
        return A2($the_sett$elm_pretty_printer$Pretty$indent, indent, A2($the_sett$elm_pretty_printer$Pretty$a, A2($the_sett$elm_pretty_printer$Pretty$indent, 4, A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, expr).a), A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string(" ->"), A2($mdgriffith$elm_codegen$Internal$Write$prettyPattern, aliases, pattern)))));
      };
      var patternsPart = $mdgriffith$elm_codegen$Internal$Write$doubleLines(A2($elm$core$List$map, prettyCase, caseBlock.il));
      var casePart = function() {
        var _v37 = A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, $mdgriffith$elm_codegen$Internal$Compiler$denode(caseBlock.iH));
        var caseExpression = _v37.a;
        var alwaysBreak = _v37.b;
        return A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
          A2($the_sett$elm_pretty_printer$Pretty$nest, indent, A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
            $the_sett$elm_pretty_printer$Pretty$string("case"),
            caseExpression
          ])))),
          $the_sett$elm_pretty_printer$Pretty$string("of")
        ])));
      }();
      return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$align($the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([casePart, patternsPart]))), true);
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyExpression = F2(function(aliases, expression) {
      return A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, expression).a;
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner = F4(function(aliases, context, indent, expression) {
      prettyExpressionInner:
        while (true) {
          var noninfix = $mdgriffith$elm_codegen$Internal$Write$showParen(context.je > 10);
          switch (expression.$) {
            case 0:
              return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$string("()"), false);
            case 1:
              var exprs = expression.a;
              return noninfix(A3($mdgriffith$elm_codegen$Internal$Write$prettyApplication, aliases, indent, exprs));
            case 2:
              var symbol = expression.a;
              var dir = expression.b;
              var exprl = expression.c;
              var exprr = expression.d;
              return A2($mdgriffith$elm_codegen$Internal$Write$showParen, _Utils_cmp(context.je, $mdgriffith$elm_codegen$Internal$Write$precedence(symbol)) > 0, A6($mdgriffith$elm_codegen$Internal$Write$prettyOperatorApplication, aliases, indent, symbol, dir, exprl, exprr));
            case 3:
              var modl = expression.a;
              var val = expression.b;
              return _Utils_Tuple2(A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string(val), A2($mdgriffith$elm_codegen$Internal$Write$prettyModuleNameDot, aliases, modl)), false);
            case 4:
              var exprBool = expression.a;
              var exprTrue = expression.b;
              var exprFalse = expression.c;
              return noninfix(A5($mdgriffith$elm_codegen$Internal$Write$prettyIfBlock, aliases, indent, exprBool, exprTrue, exprFalse));
            case 5:
              var symbol = expression.a;
              return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$parens($the_sett$elm_pretty_printer$Pretty$string(symbol)), false);
            case 6:
              var symbol = expression.a;
              return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$string(symbol), false);
            case 7:
              var val = expression.a;
              return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$string($elm$core$String$fromInt(val)), false);
            case 8:
              var val = expression.a;
              return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$string($mdgriffith$elm_codegen$Internal$Write$toHexString(val)), false);
            case 9:
              var val = expression.a;
              return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$string($elm$core$String$fromFloat(val)), false);
            case 10:
              var _v34 = expression.a;
              var expr = _v34.b;
              return noninfix(function() {
                var _v35 = A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$bottomContext, 4, expr);
                var prettyExpr = _v35.a;
                var alwaysBreak = _v35.b;
                return _Utils_Tuple2(A2($the_sett$elm_pretty_printer$Pretty$a, prettyExpr, $the_sett$elm_pretty_printer$Pretty$string("-")), alwaysBreak);
              }());
            case 11:
              var val = expression.a;
              return _Utils_Tuple2($mdgriffith$elm_codegen$Internal$Write$prettyLiteral(val), false);
            case 12:
              var val = expression.a;
              return _Utils_Tuple2($mdgriffith$elm_codegen$Internal$Write$singleQuotes($the_sett$elm_pretty_printer$Pretty$string($mdgriffith$elm_codegen$Internal$Write$escapeChar(val))), false);
            case 13:
              var exprs = expression.a;
              return A3($mdgriffith$elm_codegen$Internal$Write$prettyTupledExpression, aliases, indent, exprs);
            case 14:
              var _v36 = expression.a;
              var expr = _v36.b;
              var $temp$aliases = aliases, $temp$context = context, $temp$indent = indent, $temp$expression = expr;
              aliases = $temp$aliases;
              context = $temp$context;
              indent = $temp$indent;
              expression = $temp$expression;
              continue prettyExpressionInner;
            case 15:
              var letBlock = expression.a;
              return noninfix(A3($mdgriffith$elm_codegen$Internal$Write$prettyLetBlock, aliases, indent, letBlock));
            case 16:
              var caseBlock = expression.a;
              return noninfix(A3($mdgriffith$elm_codegen$Internal$Write$prettyCaseBlock, aliases, indent, caseBlock));
            case 17:
              var lambda = expression.a;
              return noninfix(A3($mdgriffith$elm_codegen$Internal$Write$prettyLambdaExpression, aliases, indent, lambda));
            case 18:
              var setters = expression.a;
              return A2($mdgriffith$elm_codegen$Internal$Write$prettyRecordExpr, aliases, setters);
            case 19:
              var exprs = expression.a;
              return A3($mdgriffith$elm_codegen$Internal$Write$prettyList, aliases, indent, exprs);
            case 20:
              var expr = expression.a;
              var field = expression.b;
              return A3($mdgriffith$elm_codegen$Internal$Write$prettyRecordAccess, aliases, expr, field);
            case 21:
              var field = expression.a;
              return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$string(field), false);
            case 22:
              var _var = expression.a;
              var setters = expression.b;
              return A4($mdgriffith$elm_codegen$Internal$Write$prettyRecordUpdateExpression, aliases, indent, _var, setters);
            default:
              return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$string("glsl"), true);
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyFun = F2(function(aliases, fn) {
      return $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
        A2($mdgriffith$elm_codegen$Internal$Write$prettyMaybe, $mdgriffith$elm_codegen$Internal$Write$prettyDocumentation, $mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(fn.aR)),
        A2($mdgriffith$elm_codegen$Internal$Write$prettyMaybe, $mdgriffith$elm_codegen$Internal$Write$prettySignature(aliases), $mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(fn.jq)),
        A2($mdgriffith$elm_codegen$Internal$Write$prettyFunctionImplementation, aliases, $mdgriffith$elm_codegen$Internal$Compiler$denode(fn.iw))
      ]));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyFunctionImplementation = F2(function(aliases, impl) {
      return A2($the_sett$elm_pretty_printer$Pretty$nest, 4, A2($the_sett$elm_pretty_printer$Pretty$a, A2($mdgriffith$elm_codegen$Internal$Write$prettyExpression, aliases, $mdgriffith$elm_codegen$Internal$Compiler$denode(impl.iH)), A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string($mdgriffith$elm_codegen$Internal$Compiler$denode(impl.ag)),
        A2($mdgriffith$elm_codegen$Internal$Write$prettyArgs, aliases, $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(impl.aL)),
        $the_sett$elm_pretty_printer$Pretty$string("=")
      ])))));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyIfBlock = F5(function(aliases, indent, exprBool, exprTrue, exprFalse) {
      var innerIfBlock = F3(function(_v27, _v28, _v29) {
        var innerExprBool = _v27.b;
        var innerExprTrue = _v28.b;
        var innerExprFalse = _v29.b;
        var truePart = A2($the_sett$elm_pretty_printer$Pretty$indent, indent, A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, innerExprTrue).a);
        var ifPart = function() {
          var _v32 = A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, innerExprBool);
          var alwaysBreak = _v32.b;
          return A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
            A2($the_sett$elm_pretty_printer$Pretty$nest, indent, A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
              $the_sett$elm_pretty_printer$Pretty$string("if"),
              A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, innerExprBool).a
            ])))),
            $the_sett$elm_pretty_printer$Pretty$string("then")
          ])));
        }();
        var falsePart = function() {
          if (innerExprFalse.$ === 4) {
            var nestedExprBool = innerExprFalse.a;
            var nestedExprTrue = innerExprFalse.b;
            var nestedExprFalse = innerExprFalse.c;
            return A3(innerIfBlock, nestedExprBool, nestedExprTrue, nestedExprFalse);
          } else {
            return _List_fromArray([
              A2($the_sett$elm_pretty_printer$Pretty$indent, indent, A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, innerExprFalse).a)
            ]);
          }
        }();
        var elsePart = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string("else"), $the_sett$elm_pretty_printer$Pretty$line);
        if (!falsePart.b) {
          return _List_Nil;
        } else {
          if (!falsePart.b.b) {
            var falseExpr = falsePart.a;
            return _List_fromArray([ifPart, truePart, elsePart, falseExpr]);
          } else {
            var hd = falsePart.a;
            var tl = falsePart.b;
            return A2($elm$core$List$append, _List_fromArray([
              ifPart,
              truePart,
              $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([elsePart, hd]))
            ]), tl);
          }
        }
      });
      var prettyExpressions = A3(innerIfBlock, exprBool, exprTrue, exprFalse);
      return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$align($the_sett$elm_pretty_printer$Pretty$lines(prettyExpressions)), true);
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyLambdaExpression = F3(function(aliases, indent, lambda) {
      var _v26 = A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, $mdgriffith$elm_codegen$Internal$Compiler$denode(lambda.iH));
      var prettyExpr = _v26.a;
      var alwaysBreak = _v26.b;
      return _Utils_Tuple2(A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$align(A2($the_sett$elm_pretty_printer$Pretty$nest, indent, $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
        A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string(" ->"), A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$words(A2($elm$core$List$map, A2($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(lambda.G))), $the_sett$elm_pretty_printer$Pretty$string("\\"))),
        prettyExpr
      ]))))), alwaysBreak);
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyLetBlock = F3(function(aliases, indent, letBlock) {
      return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$align($the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string("let"),
        A2($the_sett$elm_pretty_printer$Pretty$indent, indent, $mdgriffith$elm_codegen$Internal$Write$doubleLines(A2($elm$core$List$map, A2($mdgriffith$elm_codegen$Internal$Write$prettyLetDeclaration, aliases, indent), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(letBlock.ix)))),
        $the_sett$elm_pretty_printer$Pretty$string("in"),
        A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, $mdgriffith$elm_codegen$Internal$Compiler$denode(letBlock.iH)).a
      ]))), true);
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyLetDeclaration = F3(function(aliases, indent, letDecl) {
      if (!letDecl.$) {
        var fn = letDecl.a;
        return A2($mdgriffith$elm_codegen$Internal$Write$prettyFun, aliases, fn);
      } else {
        var _v24 = letDecl.a;
        var pattern = _v24.b;
        var _v25 = letDecl.b;
        var expr = _v25.b;
        return A2($the_sett$elm_pretty_printer$Pretty$a, A2($the_sett$elm_pretty_printer$Pretty$indent, indent, A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, expr).a), A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
          A3($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false, pattern),
          $the_sett$elm_pretty_printer$Pretty$string("=")
        ]))));
      }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyList = F3(function(aliases, indent, exprs) {
      if (!exprs.b) {
        return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$string("[]"), false);
      } else {
        var open = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$space, $the_sett$elm_pretty_printer$Pretty$string("["));
        var close = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string("]"), $the_sett$elm_pretty_printer$Pretty$line);
        var _v22 = A2($elm$core$Tuple$mapSecond, $elm$core$List$any($elm$core$Basics$identity), $elm$core$List$unzip(A2($elm$core$List$map, A3($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, A2($mdgriffith$elm_codegen$Internal$Write$decrementIndent, indent, 2)), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(exprs))));
        var prettyExpressions = _v22.a;
        var alwaysBreak = _v22.b;
        return _Utils_Tuple2(A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$align(A3($the_sett$elm_pretty_printer$Pretty$surround, open, close, A2($the_sett$elm_pretty_printer$Pretty$separators, ", ", prettyExpressions)))), alwaysBreak);
      }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyOperatorApplication = F6(function(aliases, indent, symbol, dir, _v15, _v16) {
      var exprl = _v15.b;
      var exprr = _v16.b;
      var prec = $mdgriffith$elm_codegen$Internal$Write$precedence(symbol);
      var _v17 = function() {
        switch (dir) {
          case 0:
            return _Utils_Tuple2(prec, prec + 1);
          case 2:
            return _Utils_Tuple2(prec + 1, prec + 1);
          default:
            return _Utils_Tuple2(prec + 1, prec);
        }
      }();
      var lprec = _v17.a;
      var rprec = _v17.b;
      var _v19 = A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, { je: rprec }, indent + 4, exprr);
      var right = _v19.a;
      var breakRight = _v19.b;
      var _v20 = A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, { je: lprec }, indent, exprl);
      var left = _v20.a;
      var breakLeft = _v20.b;
      var alwaysBreak = breakLeft || breakRight;
      return _Utils_Tuple2(A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$align(A2($the_sett$elm_pretty_printer$Pretty$a, right, A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$space, A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string(symbol), A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$space, left)))))), alwaysBreak);
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyRecordAccess = F3(function(aliases, _v12, _v13) {
      var expr = _v12.b;
      var field = _v13.b;
      var _v14 = A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$bottomContext, 4, expr);
      var prettyExpr = _v14.a;
      var alwaysBreak = _v14.b;
      return _Utils_Tuple2(A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string(field), A2($the_sett$elm_pretty_printer$Pretty$a, $mdgriffith$elm_codegen$Internal$Write$dot, prettyExpr)), alwaysBreak);
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyRecordExpr = F2(function(aliases, setters) {
      if (!setters.b) {
        return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$string("{}"), false);
      } else {
        var open = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$space, $the_sett$elm_pretty_printer$Pretty$string("{"));
        var close = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string("}"), $the_sett$elm_pretty_printer$Pretty$line);
        var _v11 = A2($elm$core$Tuple$mapSecond, $elm$core$List$any($elm$core$Basics$identity), $elm$core$List$unzip(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettySetter(aliases), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(setters))));
        var prettyExpressions = _v11.a;
        var alwaysBreak = _v11.b;
        return _Utils_Tuple2(A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$align(A3($the_sett$elm_pretty_printer$Pretty$surround, open, close, A2($the_sett$elm_pretty_printer$Pretty$separators, ", ", prettyExpressions)))), alwaysBreak);
      }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyRecordUpdateExpression = F4(function(aliases, indent, _v6, setters) {
      var _var = _v6.b;
      var addBarToFirst = function(exprs) {
        if (!exprs.b) {
          return _List_Nil;
        } else {
          var hd = exprs.a;
          var tl = exprs.b;
          return A2($elm$core$List$cons, A2($the_sett$elm_pretty_printer$Pretty$a, hd, $the_sett$elm_pretty_printer$Pretty$string("| ")), tl);
        }
      };
      if (!setters.b) {
        return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$string("{}"), false);
      } else {
        var open = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
          $the_sett$elm_pretty_printer$Pretty$string("{"),
          $the_sett$elm_pretty_printer$Pretty$string(_var)
        ])));
        var close = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string("}"), $the_sett$elm_pretty_printer$Pretty$line);
        var _v8 = A2($elm$core$Tuple$mapSecond, $elm$core$List$any($elm$core$Basics$identity), $elm$core$List$unzip(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettySetter(aliases), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(setters))));
        var prettyExpressions = _v8.a;
        var alwaysBreak = _v8.b;
        return _Utils_Tuple2(A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$align(A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$empty, close, A2($the_sett$elm_pretty_printer$Pretty$nest, indent, A2($the_sett$elm_pretty_printer$Pretty$a, A2($the_sett$elm_pretty_printer$Pretty$separators, ", ", addBarToFirst(prettyExpressions)), open))))), alwaysBreak);
      }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettySetter = F2(function(aliases, _v2) {
      var _v3 = _v2.a;
      var fld = _v3.b;
      var _v4 = _v2.b;
      var val = _v4.b;
      var _v5 = A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, val);
      var prettyExpr = _v5.a;
      var alwaysBreak = _v5.b;
      return _Utils_Tuple2(A2($the_sett$elm_pretty_printer$Pretty$nest, 4, A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
          $the_sett$elm_pretty_printer$Pretty$string(fld),
          $the_sett$elm_pretty_printer$Pretty$string("=")
        ])),
        prettyExpr
      ])))), alwaysBreak);
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyTupledExpression = F3(function(aliases, indent, exprs) {
      if (!exprs.b) {
        return _Utils_Tuple2($the_sett$elm_pretty_printer$Pretty$string("()"), false);
      } else {
        var open = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$space, $the_sett$elm_pretty_printer$Pretty$string("("));
        var close = A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string(")"), $the_sett$elm_pretty_printer$Pretty$line);
        var _v1 = A2($elm$core$Tuple$mapSecond, $elm$core$List$any($elm$core$Basics$identity), $elm$core$List$unzip(A2($elm$core$List$map, A3($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, A2($mdgriffith$elm_codegen$Internal$Write$decrementIndent, indent, 2)), $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(exprs))));
        var prettyExpressions = _v1.a;
        var alwaysBreak = _v1.b;
        return _Utils_Tuple2(A2($mdgriffith$elm_codegen$Internal$Write$optionalGroup, alwaysBreak, $the_sett$elm_pretty_printer$Pretty$align(A3($the_sett$elm_pretty_printer$Pretty$surround, open, close, A2($the_sett$elm_pretty_printer$Pretty$separators, ", ", prettyExpressions)))), alwaysBreak);
      }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyDestructuring = F3(function(aliases, pattern, expr) {
      return A2($the_sett$elm_pretty_printer$Pretty$nest, 4, $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
          A2($mdgriffith$elm_codegen$Internal$Write$prettyPattern, aliases, pattern),
          $the_sett$elm_pretty_printer$Pretty$string("=")
        ])),
        A2($mdgriffith$elm_codegen$Internal$Write$prettyExpression, aliases, expr)
      ])));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyInfix = function(infix_) {
      var dirToString = function(direction) {
        switch (direction) {
          case 0:
            return "left";
          case 1:
            return "right";
          default:
            return "non";
        }
      };
      return $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string("infix"),
        $the_sett$elm_pretty_printer$Pretty$string(dirToString($mdgriffith$elm_codegen$Internal$Compiler$denode(infix_.g))),
        $the_sett$elm_pretty_printer$Pretty$string($elm$core$String$fromInt($mdgriffith$elm_codegen$Internal$Compiler$denode(infix_.je))),
        $the_sett$elm_pretty_printer$Pretty$parens($the_sett$elm_pretty_printer$Pretty$string($mdgriffith$elm_codegen$Internal$Compiler$denode(infix_.i))),
        $the_sett$elm_pretty_printer$Pretty$string("="),
        $the_sett$elm_pretty_printer$Pretty$string($mdgriffith$elm_codegen$Internal$Compiler$denode(infix_.dX))
      ]));
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyPortDeclaration = F2(function(aliases, sig) {
      return $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string("port"),
        A2($mdgriffith$elm_codegen$Internal$Write$prettySignature, aliases, sig)
      ]));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyTypeAlias = F2(function(aliases, tAlias) {
      var typeAliasPretty = A2($the_sett$elm_pretty_printer$Pretty$nest, 4, A2($the_sett$elm_pretty_printer$Pretty$a, A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, $mdgriffith$elm_codegen$Internal$Compiler$denode(tAlias.aH)), A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string("type alias"),
        $the_sett$elm_pretty_printer$Pretty$string($mdgriffith$elm_codegen$Internal$Compiler$denode(tAlias.ag)),
        $the_sett$elm_pretty_printer$Pretty$words(A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, $mdgriffith$elm_codegen$Internal$Compiler$denodeAll(tAlias.dZ))),
        $the_sett$elm_pretty_printer$Pretty$string("=")
      ])))));
      return $the_sett$elm_pretty_printer$Pretty$lines(_List_fromArray([
        A2($mdgriffith$elm_codegen$Internal$Write$prettyMaybe, $mdgriffith$elm_codegen$Internal$Write$prettyDocumentation, $mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(tAlias.aR)),
        typeAliasPretty
      ]));
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyElmSyntaxDeclaration = F2(function(aliases, decl) {
      switch (decl.$) {
        case 0:
          var fn = decl.a;
          return A2($mdgriffith$elm_codegen$Internal$Write$prettyFun, aliases, fn);
        case 1:
          var tAlias = decl.a;
          return A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAlias, aliases, tAlias);
        case 2:
          var type_ = decl.a;
          return A2($mdgriffith$elm_codegen$Internal$Write$prettyCustomType, aliases, type_);
        case 3:
          var sig = decl.a;
          return A2($mdgriffith$elm_codegen$Internal$Write$prettyPortDeclaration, aliases, sig);
        case 4:
          var infix_ = decl.a;
          return $mdgriffith$elm_codegen$Internal$Write$prettyInfix(infix_);
        default:
          var _v1 = decl.a;
          var pattern = _v1.b;
          var _v2 = decl.b;
          var expr = _v2.b;
          return A3($mdgriffith$elm_codegen$Internal$Write$prettyDestructuring, aliases, pattern, expr);
      }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyDeclarations = F2(function(aliases, decls) {
      return A3($elm$core$List$foldl, $elm$core$Basics$apL, $the_sett$elm_pretty_printer$Pretty$empty, $elm$core$List$reverse(A2($elm$core$List$drop, 1, $elm$core$List$reverse(A2($elm$core$List$concatMap, function(decl) {
        switch (decl.$) {
          case 1:
            var content = decl.a;
            return _List_fromArray([
              $the_sett$elm_pretty_printer$Pretty$a($the_sett$elm_pretty_printer$Pretty$string(content + `
`)),
              A2($elm$core$Basics$composeR, $the_sett$elm_pretty_printer$Pretty$a($the_sett$elm_pretty_printer$Pretty$line), $the_sett$elm_pretty_printer$Pretty$a($the_sett$elm_pretty_printer$Pretty$line))
            ]);
          case 2:
            var source = decl.a;
            return _List_fromArray([
              $the_sett$elm_pretty_printer$Pretty$a($the_sett$elm_pretty_printer$Pretty$string(source)),
              A2($elm$core$Basics$composeR, $the_sett$elm_pretty_printer$Pretty$a($the_sett$elm_pretty_printer$Pretty$line), A2($elm$core$Basics$composeR, $the_sett$elm_pretty_printer$Pretty$a($the_sett$elm_pretty_printer$Pretty$line), $the_sett$elm_pretty_printer$Pretty$a($the_sett$elm_pretty_printer$Pretty$line)))
            ]);
          default:
            var innerDecl = decl.a;
            return _List_fromArray([
              $the_sett$elm_pretty_printer$Pretty$a(A2($mdgriffith$elm_codegen$Internal$Write$prettyElmSyntaxDeclaration, aliases, innerDecl)),
              A2($elm$core$Basics$composeR, $the_sett$elm_pretty_printer$Pretty$a($the_sett$elm_pretty_printer$Pretty$line), A2($elm$core$Basics$composeR, $the_sett$elm_pretty_printer$Pretty$a($the_sett$elm_pretty_printer$Pretty$line), $the_sett$elm_pretty_printer$Pretty$a($the_sett$elm_pretty_printer$Pretty$line)))
            ]);
        }
      }, decls)))));
    });
    var $mdgriffith$elm_codegen$Internal$Comments$delimeters = function(doc) {
      return A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$string("-}"), A2($the_sett$elm_pretty_printer$Pretty$a, doc, $the_sett$elm_pretty_printer$Pretty$string("{-|")));
    };
    var $mdgriffith$elm_codegen$Internal$Comments$getParts = function(_v0) {
      var parts = _v0;
      return $elm$core$List$reverse(parts);
    };
    var $mdgriffith$elm_codegen$Internal$Comments$DocTags = function(a) {
      return { $: 2, a };
    };
    var $mdgriffith$elm_codegen$Internal$Comments$fitAndSplit = F2(function(width, tags) {
      if (!tags.b) {
        return _List_Nil;
      } else {
        var t = tags.a;
        var ts = tags.b;
        var _v1 = A3($elm$core$List$foldl, F2(function(tag, _v2) {
          var allSplits = _v2.a;
          var curSplit = _v2.b;
          var remaining = _v2.c;
          return _Utils_cmp($elm$core$String$length(tag), remaining) < 1 ? _Utils_Tuple3(allSplits, A2($elm$core$List$cons, tag, curSplit), remaining - $elm$core$String$length(tag)) : _Utils_Tuple3(_Utils_ap(allSplits, _List_fromArray([
            $elm$core$List$reverse(curSplit)
          ])), _List_fromArray([tag]), width - $elm$core$String$length(tag));
        }), _Utils_Tuple3(_List_Nil, _List_fromArray([t]), width - $elm$core$String$length(t)), ts);
        var splitsExceptLast = _v1.a;
        var lastSplit = _v1.b;
        return _Utils_ap(splitsExceptLast, _List_fromArray([
          $elm$core$List$reverse(lastSplit)
        ]));
      }
    });
    var $elm$core$List$sort = function(xs) {
      return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
    };
    var $mdgriffith$elm_codegen$Internal$Comments$mergeDocTags = function(innerParts) {
      var _v0 = A3($elm$core$List$foldr, F2(function(part, _v1) {
        var accum = _v1.a;
        var context = _v1.b;
        if (context.$ === 1) {
          if (part.$ === 2) {
            var tags2 = part.a;
            return _Utils_Tuple2(accum, $elm$core$Maybe$Just(tags2));
          } else {
            var otherPart = part;
            return _Utils_Tuple2(A2($elm$core$List$cons, otherPart, accum), $elm$core$Maybe$Nothing);
          }
        } else {
          var contextTags = context.a;
          if (part.$ === 2) {
            var tags2 = part.a;
            return _Utils_Tuple2(accum, $elm$core$Maybe$Just(_Utils_ap(contextTags, tags2)));
          } else {
            var otherPart = part;
            return _Utils_Tuple2(A2($elm$core$List$cons, otherPart, A2($elm$core$List$cons, $mdgriffith$elm_codegen$Internal$Comments$DocTags($elm$core$List$sort(contextTags)), accum)), $elm$core$Maybe$Nothing);
          }
        }
      }), _Utils_Tuple2(_List_Nil, $elm$core$Maybe$Nothing), innerParts);
      var partsExceptMaybeFirst = _v0.a;
      var maybeFirstPart = _v0.b;
      if (maybeFirstPart.$ === 1) {
        return partsExceptMaybeFirst;
      } else {
        var tags = maybeFirstPart.a;
        return A2($elm$core$List$cons, $mdgriffith$elm_codegen$Internal$Comments$DocTags($elm$core$List$sort(tags)), partsExceptMaybeFirst);
      }
    };
    var $mdgriffith$elm_codegen$Internal$Comments$layoutTags = F2(function(width, parts) {
      return A3($elm$core$List$foldr, F2(function(part, _v0) {
        var accumParts = _v0.a;
        var accumDocTags = _v0.b;
        if (part.$ === 2) {
          var tags = part.a;
          var splits = A2($mdgriffith$elm_codegen$Internal$Comments$fitAndSplit, width, tags);
          return _Utils_Tuple2(_Utils_ap(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Comments$DocTags, splits), accumParts), _Utils_ap(accumDocTags, splits));
        } else {
          var otherPart = part;
          return _Utils_Tuple2(A2($elm$core$List$cons, otherPart, accumParts), accumDocTags);
        }
      }), _Utils_Tuple2(_List_Nil, _List_Nil), $mdgriffith$elm_codegen$Internal$Comments$mergeDocTags(parts));
    });
    var $the_sett$elm_pretty_printer$Internals$NLine = F3(function(a, b, c) {
      return { $: 2, a, b, c };
    });
    var $the_sett$elm_pretty_printer$Internals$NNil = { $: 0 };
    var $the_sett$elm_pretty_printer$Internals$NText = F3(function(a, b, c) {
      return { $: 1, a, b, c };
    });
    var $the_sett$elm_pretty_printer$Internals$fits = F2(function(w, normal) {
      fits:
        while (true) {
          if (w < 0) {
            return false;
          } else {
            switch (normal.$) {
              case 0:
                return true;
              case 1:
                var text = normal.a;
                var innerNormal = normal.b;
                var $temp$w = w - $elm$core$String$length(text), $temp$normal = innerNormal(0);
                w = $temp$w;
                normal = $temp$normal;
                continue fits;
              default:
                return true;
            }
          }
        }
    });
    var $the_sett$elm_pretty_printer$Internals$better = F4(function(w, k, doc, doc2Fn) {
      return A2($the_sett$elm_pretty_printer$Internals$fits, w - k, doc) ? doc : doc2Fn(0);
    });
    var $the_sett$elm_pretty_printer$Internals$best = F3(function(width, startCol, x) {
      var be = F3(function(w, k, docs) {
        be:
          while (true) {
            if (!docs.b) {
              return $the_sett$elm_pretty_printer$Internals$NNil;
            } else {
              switch (docs.a.b.$) {
                case 0:
                  var _v1 = docs.a;
                  var i = _v1.a;
                  var _v2 = _v1.b;
                  var ds = docs.b;
                  var $temp$w = w, $temp$k = k, $temp$docs = ds;
                  w = $temp$w;
                  k = $temp$k;
                  docs = $temp$docs;
                  continue be;
                case 1:
                  var _v3 = docs.a;
                  var i = _v3.a;
                  var _v4 = _v3.b;
                  var doc = _v4.a;
                  var doc2 = _v4.b;
                  var ds = docs.b;
                  var $temp$w = w, $temp$k = k, $temp$docs = A2($elm$core$List$cons, _Utils_Tuple2(i, doc(0)), A2($elm$core$List$cons, _Utils_Tuple2(i, doc2(0)), ds));
                  w = $temp$w;
                  k = $temp$k;
                  docs = $temp$docs;
                  continue be;
                case 2:
                  var _v5 = docs.a;
                  var i = _v5.a;
                  var _v6 = _v5.b;
                  var j = _v6.a;
                  var doc = _v6.b;
                  var ds = docs.b;
                  var $temp$w = w, $temp$k = k, $temp$docs = A2($elm$core$List$cons, _Utils_Tuple2(i + j, doc(0)), ds);
                  w = $temp$w;
                  k = $temp$k;
                  docs = $temp$docs;
                  continue be;
                case 3:
                  var _v7 = docs.a;
                  var i = _v7.a;
                  var _v8 = _v7.b;
                  var text = _v8.a;
                  var maybeTag = _v8.b;
                  var ds = docs.b;
                  return A3($the_sett$elm_pretty_printer$Internals$NText, text, function(_v9) {
                    return A3(be, w, k + $elm$core$String$length(text), ds);
                  }, maybeTag);
                case 4:
                  var _v10 = docs.a;
                  var i = _v10.a;
                  var _v11 = _v10.b;
                  var vsep = _v11.b;
                  var ds = docs.b;
                  return A3($the_sett$elm_pretty_printer$Internals$NLine, i, vsep, function(_v12) {
                    return A3(be, w, i + $elm$core$String$length(vsep), ds);
                  });
                case 5:
                  var _v13 = docs.a;
                  var i = _v13.a;
                  var _v14 = _v13.b;
                  var doc = _v14.a;
                  var doc2 = _v14.b;
                  var ds = docs.b;
                  return A4($the_sett$elm_pretty_printer$Internals$better, w, k, A3(be, w, k, A2($elm$core$List$cons, _Utils_Tuple2(i, doc), ds)), function(_v15) {
                    return A3(be, w, k, A2($elm$core$List$cons, _Utils_Tuple2(i, doc2), ds));
                  });
                case 6:
                  var _v16 = docs.a;
                  var i = _v16.a;
                  var fn = _v16.b.a;
                  var ds = docs.b;
                  var $temp$w = w, $temp$k = k, $temp$docs = A2($elm$core$List$cons, _Utils_Tuple2(i, fn(i)), ds);
                  w = $temp$w;
                  k = $temp$k;
                  docs = $temp$docs;
                  continue be;
                default:
                  var _v17 = docs.a;
                  var i = _v17.a;
                  var fn = _v17.b.a;
                  var ds = docs.b;
                  var $temp$w = w, $temp$k = k, $temp$docs = A2($elm$core$List$cons, _Utils_Tuple2(i, fn(k)), ds);
                  w = $temp$w;
                  k = $temp$k;
                  docs = $temp$docs;
                  continue be;
              }
            }
          }
      });
      return A3(be, width, startCol, _List_fromArray([
        _Utils_Tuple2(0, x)
      ]));
    });
    var $the_sett$elm_pretty_printer$Internals$layout = function(normal) {
      var layoutInner = F2(function(normal2, acc) {
        layoutInner:
          while (true) {
            switch (normal2.$) {
              case 0:
                return acc;
              case 1:
                var text = normal2.a;
                var innerNormal = normal2.b;
                var maybeTag = normal2.c;
                var $temp$normal2 = innerNormal(0), $temp$acc = A2($elm$core$List$cons, text, acc);
                normal2 = $temp$normal2;
                acc = $temp$acc;
                continue layoutInner;
              default:
                var i = normal2.a;
                var sep = normal2.b;
                var innerNormal = normal2.c;
                var norm = innerNormal(0);
                if (norm.$ === 2) {
                  var $temp$normal2 = innerNormal(0), $temp$acc = A2($elm$core$List$cons, `
` + sep, acc);
                  normal2 = $temp$normal2;
                  acc = $temp$acc;
                  continue layoutInner;
                } else {
                  var $temp$normal2 = innerNormal(0), $temp$acc = A2($elm$core$List$cons, `
` + (A2($the_sett$elm_pretty_printer$Internals$copy, i, " ") + sep), acc);
                  normal2 = $temp$normal2;
                  acc = $temp$acc;
                  continue layoutInner;
                }
            }
          }
      });
      return $elm$core$String$concat($elm$core$List$reverse(A2(layoutInner, normal, _List_Nil)));
    };
    var $the_sett$elm_pretty_printer$Pretty$pretty = F2(function(w, doc) {
      return $the_sett$elm_pretty_printer$Internals$layout(A3($the_sett$elm_pretty_printer$Internals$best, w, 0, doc));
    });
    var $mdgriffith$elm_codegen$Internal$Comments$prettyCode = function(val) {
      return A2($the_sett$elm_pretty_printer$Pretty$indent, 4, $the_sett$elm_pretty_printer$Pretty$string(val));
    };
    var $mdgriffith$elm_codegen$Internal$Comments$prettyMarkdown = function(val) {
      return $the_sett$elm_pretty_printer$Pretty$string(val);
    };
    var $mdgriffith$elm_codegen$Internal$Comments$prettyTags = function(tags) {
      return $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string("@docs"),
        A2($the_sett$elm_pretty_printer$Pretty$join, $the_sett$elm_pretty_printer$Pretty$string(", "), A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, tags))
      ]));
    };
    var $mdgriffith$elm_codegen$Internal$Comments$prettyCommentPart = function(part) {
      switch (part.$) {
        case 0:
          var val = part.a;
          return $mdgriffith$elm_codegen$Internal$Comments$prettyMarkdown(val);
        case 1:
          var val = part.a;
          return $mdgriffith$elm_codegen$Internal$Comments$prettyCode(val);
        default:
          var tags = part.a;
          return $mdgriffith$elm_codegen$Internal$Comments$prettyTags(tags);
      }
    };
    var $mdgriffith$elm_codegen$Internal$Comments$prettyFileComment = F2(function(width, comment) {
      var _v0 = A2($mdgriffith$elm_codegen$Internal$Comments$layoutTags, width, $mdgriffith$elm_codegen$Internal$Comments$getParts(comment));
      var parts = _v0.a;
      var splits = _v0.b;
      return _Utils_Tuple2(A2($the_sett$elm_pretty_printer$Pretty$pretty, width, $mdgriffith$elm_codegen$Internal$Comments$delimeters($the_sett$elm_pretty_printer$Pretty$lines(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Comments$prettyCommentPart, parts)))), splits);
    });
    var $elm$core$Basics$ge = _Utils_ge;
    var $mdgriffith$elm_codegen$Internal$Write$checkIfIsIndented = F2(function(tll, count) {
      checkIfIsIndented:
        while (true) {
          if (count >= 5) {
            return true;
          } else {
            if (!tll.b) {
              return false;
            } else {
              var _v1 = tll.a;
              var range = _v1.a;
              var xs = tll.b;
              if (range.z.jn > 0) {
                return true;
              } else {
                var $temp$tll = xs, $temp$count = count + 1;
                tll = $temp$tll;
                count = $temp$count;
                continue checkIfIsIndented;
              }
            }
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyGroupedExposing = F2(function(_v0, rendered) {
      var range = _v0.a;
      var exposedElement = _v0.b;
      var renderedTopElement = $mdgriffith$elm_codegen$Internal$Write$prettyTopLevelExpose(exposedElement);
      var currentRow = range.z.jn;
      var newRow = !_Utils_eq(currentRow, rendered.bb) || rendered.aD > 5;
      var newRendered = rendered.aV ? renderedTopElement : newRow ? A2($the_sett$elm_pretty_printer$Pretty$append, $the_sett$elm_pretty_printer$Pretty$string(`
    , `), renderedTopElement) : A2($the_sett$elm_pretty_printer$Pretty$append, $the_sett$elm_pretty_printer$Pretty$string(", "), renderedTopElement);
      return {
        aV: false,
        bb: currentRow,
        bc: A2($the_sett$elm_pretty_printer$Pretty$append, rendered.bc, newRendered),
        aD: newRow ? 0 : rendered.aD + 1
      };
    });
    var $mdgriffith$elm_codegen$Internal$Write$prettyModuleExposing = function(exposing_) {
      var exposings = function() {
        if (!exposing_.$) {
          return A3($the_sett$elm_pretty_printer$Pretty$surround, $the_sett$elm_pretty_printer$Pretty$string(" ("), $the_sett$elm_pretty_printer$Pretty$string(")"), $the_sett$elm_pretty_printer$Pretty$string(".."));
        } else {
          var tll = exposing_.a;
          var isIndented = A2($mdgriffith$elm_codegen$Internal$Write$checkIfIsIndented, tll, 0);
          var start = isIndented ? $the_sett$elm_pretty_printer$Pretty$string(`
    ( `) : $the_sett$elm_pretty_printer$Pretty$string(" ( ");
          var end = isIndented ? $the_sett$elm_pretty_printer$Pretty$string(`
    )`) : $the_sett$elm_pretty_printer$Pretty$string(" )");
          return A2($the_sett$elm_pretty_printer$Pretty$append, A3($elm$core$List$foldr, $mdgriffith$elm_codegen$Internal$Write$prettyGroupedExposing, { aV: true, bb: 0, bc: start, aD: 0 }, tll).bc, end);
        }
      }();
      return A2($the_sett$elm_pretty_printer$Pretty$a, exposings, $the_sett$elm_pretty_printer$Pretty$string("exposing"));
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyDefaultModuleData = function(moduleData) {
      return $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string("module"),
        $mdgriffith$elm_codegen$Internal$Write$prettyModuleName($mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.a3)),
        $mdgriffith$elm_codegen$Internal$Write$prettyModuleExposing($mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.dv))
      ]));
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyEffectModuleData = function(moduleData) {
      var prettyCmdAndSub = F2(function(maybeCmd, maybeSub) {
        var _v0 = _Utils_Tuple2(maybeCmd, maybeSub);
        if (!_v0.a.$) {
          if (!_v0.b.$) {
            var cmdName = _v0.a.a;
            var subName = _v0.b.a;
            return $elm$core$Maybe$Just($the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
              $the_sett$elm_pretty_printer$Pretty$string("where { command ="),
              $the_sett$elm_pretty_printer$Pretty$string(cmdName),
              $the_sett$elm_pretty_printer$Pretty$string(","),
              $the_sett$elm_pretty_printer$Pretty$string("subscription ="),
              $the_sett$elm_pretty_printer$Pretty$string(subName),
              $the_sett$elm_pretty_printer$Pretty$string("}")
            ])));
          } else {
            var cmdName = _v0.a.a;
            var _v3 = _v0.b;
            return $elm$core$Maybe$Just($the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
              $the_sett$elm_pretty_printer$Pretty$string("where { command ="),
              $the_sett$elm_pretty_printer$Pretty$string(cmdName),
              $the_sett$elm_pretty_printer$Pretty$string("}")
            ])));
          }
        } else {
          if (_v0.b.$ === 1) {
            var _v1 = _v0.a;
            var _v2 = _v0.b;
            return $elm$core$Maybe$Nothing;
          } else {
            var _v4 = _v0.a;
            var subName = _v0.b.a;
            return $elm$core$Maybe$Just($the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
              $the_sett$elm_pretty_printer$Pretty$string("where { subscription ="),
              $the_sett$elm_pretty_printer$Pretty$string(subName),
              $the_sett$elm_pretty_printer$Pretty$string("}")
            ])));
          }
        }
      });
      return $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string("effect module"),
        $mdgriffith$elm_codegen$Internal$Write$prettyModuleName($mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.a3)),
        A2($mdgriffith$elm_codegen$Internal$Write$prettyMaybe, $elm$core$Basics$identity, A2(prettyCmdAndSub, $mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(moduleData.ir), $mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(moduleData.ju))),
        $mdgriffith$elm_codegen$Internal$Write$prettyModuleExposing($mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.dv))
      ]));
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyPortModuleData = function(moduleData) {
      return $the_sett$elm_pretty_printer$Pretty$words(_List_fromArray([
        $the_sett$elm_pretty_printer$Pretty$string("port module"),
        $mdgriffith$elm_codegen$Internal$Write$prettyModuleName($mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.a3)),
        $mdgriffith$elm_codegen$Internal$Write$prettyModuleExposing($mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.dv))
      ]));
    };
    var $mdgriffith$elm_codegen$Internal$Write$prettyModule = function(mod) {
      switch (mod.$) {
        case 0:
          var defaultModuleData = mod.a;
          return $mdgriffith$elm_codegen$Internal$Write$prettyDefaultModuleData(defaultModuleData);
        case 1:
          var defaultModuleData = mod.a;
          return $mdgriffith$elm_codegen$Internal$Write$prettyPortModuleData(defaultModuleData);
        default:
          var effectModuleData = mod.a;
          return $mdgriffith$elm_codegen$Internal$Write$prettyEffectModuleData(effectModuleData);
      }
    };
    var $mdgriffith$elm_codegen$Internal$Write$prepareLayout = F2(function(width, file) {
      return A2($the_sett$elm_pretty_printer$Pretty$a, A2($mdgriffith$elm_codegen$Internal$Write$prettyDeclarations, file.ib, file.ix), A2($the_sett$elm_pretty_printer$Pretty$a, $mdgriffith$elm_codegen$Internal$Write$importsPretty(file.c), function(doc) {
        var _v0 = file.is;
        if (_v0.$ === 1) {
          return doc;
        } else {
          var fileComment = _v0.a;
          var _v1 = A2($mdgriffith$elm_codegen$Internal$Comments$prettyFileComment, width, fileComment);
          var fileCommentStr = _v1.a;
          return A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, A2($the_sett$elm_pretty_printer$Pretty$a, $mdgriffith$elm_codegen$Internal$Write$prettyComments(_List_fromArray([fileCommentStr])), doc));
        }
      }(A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, $mdgriffith$elm_codegen$Internal$Write$prettyModule(file.i1))))));
    });
    var $mdgriffith$elm_codegen$Internal$Write$pretty = F2(function(width, file) {
      return A2($the_sett$elm_pretty_printer$Pretty$pretty, width, A2($mdgriffith$elm_codegen$Internal$Write$prepareLayout, width, file));
    });
    var $mdgriffith$elm_codegen$Internal$Write$write = $mdgriffith$elm_codegen$Internal$Write$pretty(80);
    var $mdgriffith$elm_codegen$Internal$Render$render = F2(function(initialDocs, fileDetails) {
      var rendered = A3($elm$core$List$foldl, $mdgriffith$elm_codegen$Internal$Render$renderDecls(fileDetails), {
        ix: _List_Nil,
        I: _List_fromArray([0]),
        du: _List_Nil,
        ad: false,
        c: _List_Nil,
        h0: _List_Nil
      }, fileDetails.ix);
      var body = $mdgriffith$elm_codegen$Internal$Write$write({
        ib: fileDetails.ib,
        is: function() {
          var exposedGroups = A3($elm$core$List$foldl, $mdgriffith$elm_codegen$Internal$Render$getExposedGroups, _List_Nil, fileDetails.ix);
          var docCommentString = A4($mdgriffith$elm_codegen$Internal$Render$exposedGroupToMarkdown, function() {
            var _v0 = rendered.du;
            if (!_v0.b) {
              return 1;
            } else {
              return 0;
            }
          }(), $elm$core$List$reverse(exposedGroups), $mdgriffith$elm_codegen$Internal$Render$Normal, initialDocs);
          return $elm$core$String$trim(docCommentString) === "" ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Internal$Comments$addPart, $mdgriffith$elm_codegen$Internal$Comments$emptyComment, $mdgriffith$elm_codegen$Internal$Comments$Markdown(`
` + (docCommentString + `
`))));
        }(),
        ix: $elm$core$List$reverse(rendered.ix),
        c: A2($elm$core$List$filterMap, A2($mdgriffith$elm_codegen$Internal$Compiler$makeImport, fileDetails.a3, fileDetails.ib), $mdgriffith$elm_codegen$Internal$Render$dedupImports(rendered.c)),
        i1: (rendered.ad ? $stil4m$elm_syntax$Elm$Syntax$Module$PortModule : $stil4m$elm_syntax$Elm$Syntax$Module$NormalModule)({
          dv: function() {
            var _v1 = rendered.du;
            if (!_v1.b) {
              return $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$All($stil4m$elm_syntax$Elm$Syntax$Range$emptyRange));
            } else {
              return $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit($elm$core$List$concat(A2($elm$core$List$indexedMap, $mdgriffith$elm_codegen$Internal$Render$groupExposedItems, rendered.du))));
            }
          }(),
          a3: $mdgriffith$elm_codegen$Internal$Compiler$nodify(fileDetails.a3)
        })
      });
      return {
        cv: body,
        f_: A2($elm$core$String$join, "/", fileDetails.a3) + ".elm",
        h0: rendered.h0
      };
    });
    var $mdgriffith$elm_codegen$Internal$Index$Index = F5(function(a, b, c, d, e) {
      return { $: 0, a, b, c, d, e };
    });
    var $mdgriffith$elm_codegen$Internal$Index$startIndex = function(modName) {
      return A5($mdgriffith$elm_codegen$Internal$Index$Index, modName, 0, _List_Nil, $elm$core$Set$empty, true);
    };
    var $mdgriffith$elm_codegen$Elm$file = F2(function(mod, decs) {
      return A2($mdgriffith$elm_codegen$Internal$Render$render, "", {
        ib: _List_Nil,
        ix: decs,
        at: $mdgriffith$elm_codegen$Internal$Index$startIndex($elm$core$Maybe$Just(mod)),
        a3: mod
      });
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$Annotation = $elm$core$Basics$identity;
    var $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases = $elm$core$Dict$empty;
    var $mdgriffith$elm_codegen$Internal$Format$formatType = function(str) {
      return _Utils_ap($elm$core$String$toUpper(A2($elm$core$String$left, 1, str)), A2($elm$core$String$dropLeft, 1, str));
    };
    var $mdgriffith$elm_codegen$Elm$Annotation$getAliases = function(_v0) {
      var ann = _v0;
      return ann.ib;
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports = function(_v0) {
      var details = _v0;
      return details.c;
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation = function(_v0) {
      var details = _v0;
      return details.bM;
    };
    var $elm$core$Dict$union = F2(function(t1, t2) {
      return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$mergeAliases = $elm$core$Dict$union;
    var $mdgriffith$elm_codegen$Internal$Compiler$nodifyAll = $elm$core$List$map($mdgriffith$elm_codegen$Internal$Compiler$nodify);
    var $mdgriffith$elm_codegen$Elm$Annotation$namedWith = F3(function(mod, name, args) {
      return {
        ib: A3($elm$core$List$foldl, F2(function(ann, aliases) {
          return A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, $mdgriffith$elm_codegen$Elm$Annotation$getAliases(ann), aliases);
        }), $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, args),
        bM: A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2(mod, $mdgriffith$elm_codegen$Internal$Format$formatType(name))), $mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation, args))),
        c: A2($elm$core$List$cons, mod, A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, args))
      };
    });
    var $author$project$Gen$Json$Encode$annotation_ = {
      hU: A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil)
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$formatAliasKey = F2(function(mod, name) {
      return A2($elm$core$String$join, ".", mod) + ("." + name);
    });
    var $elm$core$List$member = F2(function(x, xs) {
      return A2($elm$core$List$any, function(a) {
        return _Utils_eq(a, x);
      }, xs);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$uniqueHelp = F2(function(remaining, accumulator) {
      uniqueHelp:
        while (true) {
          if (!remaining.b) {
            return $elm$core$List$reverse(accumulator);
          } else {
            var first = remaining.a;
            var rest = remaining.b;
            if (A2($elm$core$List$member, first, accumulator)) {
              var $temp$remaining = rest, $temp$accumulator = accumulator;
              remaining = $temp$remaining;
              accumulator = $temp$accumulator;
              continue uniqueHelp;
            } else {
              var $temp$remaining = rest, $temp$accumulator = A2($elm$core$List$cons, first, accumulator);
              remaining = $temp$remaining;
              accumulator = $temp$accumulator;
              continue uniqueHelp;
            }
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$unique = function(list) {
      return A2($mdgriffith$elm_codegen$Internal$Compiler$uniqueHelp, list, _List_Nil);
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$getGenerics = function(_v0) {
      var details = _v0;
      return $mdgriffith$elm_codegen$Internal$Compiler$unique($mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper(details.bM));
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$addAlias = F4(function(mod, name, ann, aliasCache) {
      var annDetails = ann;
      return A3($elm$core$Dict$insert, A2($mdgriffith$elm_codegen$Internal$Compiler$formatAliasKey, mod, name), {
        hf: annDetails.bM,
        bs: $mdgriffith$elm_codegen$Internal$Compiler$getGenerics(ann)
      }, aliasCache);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$getAliases = function(_v0) {
      var ann = _v0;
      return ann.ib;
    };
    var $mdgriffith$elm_codegen$Elm$Annotation$alias = F4(function(mod, name, vars, target) {
      return {
        ib: A4($mdgriffith$elm_codegen$Internal$Compiler$addAlias, mod, name, target, A3($elm$core$List$foldl, F2(function(ann, aliases) {
          return A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, $mdgriffith$elm_codegen$Internal$Compiler$getAliases(ann), aliases);
        }), $mdgriffith$elm_codegen$Internal$Compiler$getAliases(target), vars)),
        bM: A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2(mod, $mdgriffith$elm_codegen$Internal$Format$formatType(name))), A2($elm$core$List$map, A2($elm$core$Basics$composeL, $mdgriffith$elm_codegen$Internal$Compiler$nodify, $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation), vars)),
        c: function() {
          if (!mod.b) {
            return A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, vars);
          } else {
            return A2($elm$core$List$cons, mod, A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, vars));
          }
        }()
      };
    });
    var $author$project$Gen$Html$moduleName_ = _List_fromArray(["Html"]);
    var $mdgriffith$elm_codegen$Elm$Annotation$var = function(a) {
      return {
        ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
        bM: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType($mdgriffith$elm_codegen$Internal$Format$formatValue(a)),
        c: _List_Nil
      };
    };
    var $author$project$Gen$Html$annotation_ = {
      $8: function(attributeArg0) {
        return A4($mdgriffith$elm_codegen$Elm$Annotation$alias, $author$project$Gen$Html$moduleName_, "Attribute", _List_fromArray([attributeArg0]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["VirtualDom"]), "Attribute", _List_fromArray([
          $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
        ])));
      },
      iP: function(htmlArg0) {
        return A4($mdgriffith$elm_codegen$Elm$Annotation$alias, $author$project$Gen$Html$moduleName_, "Html", _List_fromArray([htmlArg0]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["VirtualDom"]), "Node", _List_fromArray([
          $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
        ])));
      }
    };
    var $author$project$Ui$annotation_ = { c5: $author$project$Gen$Html$annotation_.iP };
    var $stil4m$elm_syntax$Elm$Syntax$Expression$Application = function(a) {
      return { $: 1, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$FunctionAppliedToTooManyArgs = F2(function(a, b) {
      return { $: 3, a, b };
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$containsFieldByName = F2(function(_v0, _v2) {
      var _v1 = _v0.a;
      var oneName = _v1.b;
      var _v3 = _v2.a;
      var twoName = _v3.b;
      return _Utils_eq(oneName, twoName);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$mergeFieldLists = F2(function(fieldOne, fieldTwo) {
      return A3($elm$core$List$foldl, F2(function(_new, existing) {
        var newField = _new.b;
        return A2($elm$core$List$any, A2($elm$core$Basics$composeL, $mdgriffith$elm_codegen$Internal$Compiler$containsFieldByName(newField), $mdgriffith$elm_codegen$Internal$Compiler$denode), existing) ? existing : A2($elm$core$List$cons, _new, existing);
      }), fieldOne, fieldTwo);
    });
    var $elm$core$Dict$getMin = function(dict) {
      getMin:
        while (true) {
          if (dict.$ === -1 && dict.d.$ === -1) {
            var left = dict.d;
            var $temp$dict = left;
            dict = $temp$dict;
            continue getMin;
          } else {
            return dict;
          }
        }
    };
    var $elm$core$Dict$moveRedLeft = function(dict) {
      if (dict.$ === -1 && dict.d.$ === -1 && dict.e.$ === -1) {
        if (dict.e.d.$ === -1 && !dict.e.d.a) {
          var clr = dict.a;
          var k = dict.b;
          var v = dict.c;
          var _v1 = dict.d;
          var lClr = _v1.a;
          var lK = _v1.b;
          var lV = _v1.c;
          var lLeft = _v1.d;
          var lRight = _v1.e;
          var _v2 = dict.e;
          var rClr = _v2.a;
          var rK = _v2.b;
          var rV = _v2.c;
          var rLeft = _v2.d;
          var _v3 = rLeft.a;
          var rlK = rLeft.b;
          var rlV = rLeft.c;
          var rlL = rLeft.d;
          var rlR = rLeft.e;
          var rRight = _v2.e;
          return A5($elm$core$Dict$RBNode_elm_builtin, 0, rlK, rlV, A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight), rlL), A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
        } else {
          var clr = dict.a;
          var k = dict.b;
          var v = dict.c;
          var _v4 = dict.d;
          var lClr = _v4.a;
          var lK = _v4.b;
          var lV = _v4.c;
          var lLeft = _v4.d;
          var lRight = _v4.e;
          var _v5 = dict.e;
          var rClr = _v5.a;
          var rK = _v5.b;
          var rV = _v5.c;
          var rLeft = _v5.d;
          var rRight = _v5.e;
          if (clr === 1) {
            return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight), A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
          } else {
            return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight), A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
          }
        }
      } else {
        return dict;
      }
    };
    var $elm$core$Dict$moveRedRight = function(dict) {
      if (dict.$ === -1 && dict.d.$ === -1 && dict.e.$ === -1) {
        if (dict.d.d.$ === -1 && !dict.d.d.a) {
          var clr = dict.a;
          var k = dict.b;
          var v = dict.c;
          var _v1 = dict.d;
          var lClr = _v1.a;
          var lK = _v1.b;
          var lV = _v1.c;
          var _v2 = _v1.d;
          var _v3 = _v2.a;
          var llK = _v2.b;
          var llV = _v2.c;
          var llLeft = _v2.d;
          var llRight = _v2.e;
          var lRight = _v1.e;
          var _v4 = dict.e;
          var rClr = _v4.a;
          var rK = _v4.b;
          var rV = _v4.c;
          var rLeft = _v4.d;
          var rRight = _v4.e;
          return A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight), A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, lRight, A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
        } else {
          var clr = dict.a;
          var k = dict.b;
          var v = dict.c;
          var _v5 = dict.d;
          var lClr = _v5.a;
          var lK = _v5.b;
          var lV = _v5.c;
          var lLeft = _v5.d;
          var lRight = _v5.e;
          var _v6 = dict.e;
          var rClr = _v6.a;
          var rK = _v6.b;
          var rV = _v6.c;
          var rLeft = _v6.d;
          var rRight = _v6.e;
          if (clr === 1) {
            return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight), A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
          } else {
            return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight), A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
          }
        }
      } else {
        return dict;
      }
    };
    var $elm$core$Dict$removeHelpPrepEQGT = F7(function(targetKey, dict, color, key, value, left, right) {
      if (left.$ === -1 && !left.a) {
        var _v1 = left.a;
        var lK = left.b;
        var lV = left.c;
        var lLeft = left.d;
        var lRight = left.e;
        return A5($elm$core$Dict$RBNode_elm_builtin, color, lK, lV, lLeft, A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
      } else {
        _v2$2:
          while (true) {
            if (right.$ === -1 && right.a === 1) {
              if (right.d.$ === -1) {
                if (right.d.a === 1) {
                  var _v3 = right.a;
                  var _v4 = right.d;
                  var _v5 = _v4.a;
                  return $elm$core$Dict$moveRedRight(dict);
                } else {
                  break _v2$2;
                }
              } else {
                var _v6 = right.a;
                var _v7 = right.d;
                return $elm$core$Dict$moveRedRight(dict);
              }
            } else {
              break _v2$2;
            }
          }
        return dict;
      }
    });
    var $elm$core$Dict$removeMin = function(dict) {
      if (dict.$ === -1 && dict.d.$ === -1) {
        var color = dict.a;
        var key = dict.b;
        var value = dict.c;
        var left = dict.d;
        var lColor = left.a;
        var lLeft = left.d;
        var right = dict.e;
        if (lColor === 1) {
          if (lLeft.$ === -1 && !lLeft.a) {
            var _v3 = lLeft.a;
            return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, $elm$core$Dict$removeMin(left), right);
          } else {
            var _v4 = $elm$core$Dict$moveRedLeft(dict);
            if (_v4.$ === -1) {
              var nColor = _v4.a;
              var nKey = _v4.b;
              var nValue = _v4.c;
              var nLeft = _v4.d;
              var nRight = _v4.e;
              return A5($elm$core$Dict$balance, nColor, nKey, nValue, $elm$core$Dict$removeMin(nLeft), nRight);
            } else {
              return $elm$core$Dict$RBEmpty_elm_builtin;
            }
          }
        } else {
          return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, $elm$core$Dict$removeMin(left), right);
        }
      } else {
        return $elm$core$Dict$RBEmpty_elm_builtin;
      }
    };
    var $elm$core$Dict$removeHelp = F2(function(targetKey, dict) {
      if (dict.$ === -2) {
        return $elm$core$Dict$RBEmpty_elm_builtin;
      } else {
        var color = dict.a;
        var key = dict.b;
        var value = dict.c;
        var left = dict.d;
        var right = dict.e;
        if (_Utils_cmp(targetKey, key) < 0) {
          if (left.$ === -1 && left.a === 1) {
            var _v4 = left.a;
            var lLeft = left.d;
            if (lLeft.$ === -1 && !lLeft.a) {
              var _v6 = lLeft.a;
              return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, A2($elm$core$Dict$removeHelp, targetKey, left), right);
            } else {
              var _v7 = $elm$core$Dict$moveRedLeft(dict);
              if (_v7.$ === -1) {
                var nColor = _v7.a;
                var nKey = _v7.b;
                var nValue = _v7.c;
                var nLeft = _v7.d;
                var nRight = _v7.e;
                return A5($elm$core$Dict$balance, nColor, nKey, nValue, A2($elm$core$Dict$removeHelp, targetKey, nLeft), nRight);
              } else {
                return $elm$core$Dict$RBEmpty_elm_builtin;
              }
            }
          } else {
            return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, A2($elm$core$Dict$removeHelp, targetKey, left), right);
          }
        } else {
          return A2($elm$core$Dict$removeHelpEQGT, targetKey, A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
        }
      }
    });
    var $elm$core$Dict$removeHelpEQGT = F2(function(targetKey, dict) {
      if (dict.$ === -1) {
        var color = dict.a;
        var key = dict.b;
        var value = dict.c;
        var left = dict.d;
        var right = dict.e;
        if (_Utils_eq(targetKey, key)) {
          var _v1 = $elm$core$Dict$getMin(right);
          if (_v1.$ === -1) {
            var minKey = _v1.b;
            var minValue = _v1.c;
            return A5($elm$core$Dict$balance, color, minKey, minValue, left, $elm$core$Dict$removeMin(right));
          } else {
            return $elm$core$Dict$RBEmpty_elm_builtin;
          }
        } else {
          return A5($elm$core$Dict$balance, color, key, value, left, A2($elm$core$Dict$removeHelp, targetKey, right));
        }
      } else {
        return $elm$core$Dict$RBEmpty_elm_builtin;
      }
    });
    var $elm$core$Dict$remove = F2(function(key, dict) {
      var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
      if (_v0.$ === -1 && !_v0.a) {
        var _v1 = _v0.a;
        var k = _v0.b;
        var v = _v0.c;
        var l = _v0.d;
        var r = _v0.e;
        return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
      } else {
        var x = _v0;
        return x;
      }
    });
    var $elm$core$Dict$update = F3(function(targetKey, alter, dictionary) {
      var _v0 = alter(A2($elm$core$Dict$get, targetKey, dictionary));
      if (!_v0.$) {
        var value = _v0.a;
        return A3($elm$core$Dict$insert, targetKey, value, dictionary);
      } else {
        return A2($elm$core$Dict$remove, targetKey, dictionary);
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$addInference = F3(function(key, value, infs) {
      return A3($elm$core$Dict$update, key, function(maybeValue) {
        if (maybeValue.$ === 1) {
          return $elm$core$Maybe$Just(value);
        } else {
          if (maybeValue.a.$ === 5) {
            var _v1 = maybeValue.a;
            var _v2 = _v1.a;
            var range = _v2.a;
            var recordName = _v2.b;
            var _v3 = _v1.b;
            var fieldRange = _v3.a;
            var fields = _v3.b;
            if (value.$ === 5) {
              var _v5 = value.b;
              var existingFields = _v5.b;
              return $elm$core$Maybe$Just(A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord, A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, recordName), A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fieldRange, A2($mdgriffith$elm_codegen$Internal$Compiler$mergeFieldLists, fields, existingFields))));
            } else {
              return maybeValue;
            }
          } else {
            var existing = maybeValue.a;
            return $elm$core$Maybe$Just(existing);
          }
        }
      }, infs);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$makeFunctionReversedHelper = F2(function(last, reversedArgs) {
      makeFunctionReversedHelper:
        while (true) {
          if (!reversedArgs.b) {
            return last;
          } else {
            if (!reversedArgs.b.b) {
              var penUlt = reversedArgs.a;
              return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, penUlt), A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, last));
            } else {
              var penUlt = reversedArgs.a;
              var remain = reversedArgs.b;
              var $temp$last = A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, penUlt), A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, last)), $temp$reversedArgs = remain;
              last = $temp$last;
              reversedArgs = $temp$reversedArgs;
              continue makeFunctionReversedHelper;
            }
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$makeFunction = F2(function(result, args) {
      return A2($mdgriffith$elm_codegen$Internal$Compiler$makeFunctionReversedHelper, result, $elm$core$List$reverse(args));
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$MismatchedTypeVariables = { $: 4 };
    var $mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify = F2(function(a, b) {
      return { $: 14, a, b };
    });
    var $elm$core$Dict$fromList = function(assocs) {
      return A3($elm$core$List$foldl, F2(function(_v0, dict) {
        var key = _v0.a;
        var value = _v0.b;
        return A3($elm$core$Dict$insert, key, value, dict);
      }), $elm$core$Dict$empty, assocs);
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$getAlias = F2(function(_v0, cache) {
      var _v1 = _v0.b;
      var modName = _v1.a;
      var name = _v1.b;
      return A2($elm$core$Dict$get, A2($mdgriffith$elm_codegen$Internal$Compiler$formatAliasKey, modName, name), cache);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$CouldNotFindField = function(a) {
      return { $: 7, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$getField = F4(function(name, val, fields, captured) {
      getField:
        while (true) {
          if (!fields.b) {
            return $elm$core$Result$Err($mdgriffith$elm_codegen$Internal$Compiler$CouldNotFindField({
              iE: A2($elm$core$List$map, A2($elm$core$Basics$composeR, $mdgriffith$elm_codegen$Internal$Compiler$denode, A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $mdgriffith$elm_codegen$Internal$Compiler$denode)), captured),
              ar: name
            }));
          } else {
            var top = fields.a;
            var remain = fields.b;
            var _v1 = $mdgriffith$elm_codegen$Internal$Compiler$denode(top);
            var _v2 = _v1.a;
            var topName = _v2.b;
            var _v3 = _v1.b;
            var topVal = _v3.b;
            if (_Utils_eq(topName, name)) {
              return $elm$core$Result$Ok(_Utils_Tuple2(topVal, _Utils_ap(captured, remain)));
            } else {
              var $temp$name = name, $temp$val = val, $temp$fields = remain, $temp$captured = A2($elm$core$List$cons, top, captured);
              name = $temp$name;
              val = $temp$val;
              fields = $temp$fields;
              captured = $temp$captured;
              continue getField;
            }
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$unifiable = F4(function(aliases, vars, one, two) {
      unifiable:
        while (true) {
          switch (one.$) {
            case 0:
              var varName = one.a;
              var _v28 = A2($elm$core$Dict$get, varName, vars);
              if (_v28.$ === 1) {
                if (!two.$) {
                  var varNameB = two.a;
                  return _Utils_eq(varNameB, varName) ? _Utils_Tuple2(vars, $elm$core$Result$Ok(one)) : _Utils_Tuple2(A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, varName, two, vars), $elm$core$Result$Ok(two));
                } else {
                  return _Utils_Tuple2(A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, varName, two, vars), $elm$core$Result$Ok(two));
                }
              } else {
                var found = _v28.a;
                if (!two.$) {
                  var varNameB = two.a;
                  if (_Utils_eq(varNameB, varName)) {
                    return _Utils_Tuple2(vars, $elm$core$Result$Ok(one));
                  } else {
                    var _v31 = A2($elm$core$Dict$get, varNameB, vars);
                    if (_v31.$ === 1) {
                      return _Utils_Tuple2(A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, varNameB, found, vars), $elm$core$Result$Ok(two));
                    } else {
                      var foundTwo = _v31.a;
                      var $temp$aliases = aliases, $temp$vars = vars, $temp$one = found, $temp$two = foundTwo;
                      aliases = $temp$aliases;
                      vars = $temp$vars;
                      one = $temp$one;
                      two = $temp$two;
                      continue unifiable;
                    }
                  }
                } else {
                  var $temp$aliases = aliases, $temp$vars = vars, $temp$one = found, $temp$two = two;
                  aliases = $temp$aliases;
                  vars = $temp$vars;
                  one = $temp$one;
                  two = $temp$two;
                  continue unifiable;
                }
              }
            case 1:
              var oneName = one.a;
              var oneVars = one.b;
              switch (two.$) {
                case 1:
                  var twoName = two.a;
                  var twoContents = two.b;
                  if (_Utils_eq($mdgriffith$elm_codegen$Internal$Compiler$denode(oneName), $mdgriffith$elm_codegen$Internal$Compiler$denode(twoName))) {
                    var _v33 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableLists, aliases, vars, oneVars, twoContents, _List_Nil);
                    if (!_v33.b.$) {
                      var newVars = _v33.a;
                      var unifiedContent = _v33.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Ok(A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, twoName, unifiedContent)));
                    } else {
                      var newVars = _v33.a;
                      var err = _v33.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Err(err));
                    }
                  } else {
                    return _Utils_Tuple2(vars, $elm$core$Result$Err(A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
                  }
                case 0:
                  var b = two.a;
                  return _Utils_Tuple2(A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars), $elm$core$Result$Ok(one));
                default:
                  var _v34 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifyWithAlias, aliases, vars, oneName, oneVars, two);
                  if (_v34.$ === 1) {
                    return _Utils_Tuple2(vars, $elm$core$Result$Err(A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
                  } else {
                    var unified = _v34.a;
                    return unified;
                  }
              }
            case 2:
              switch (two.$) {
                case 0:
                  var b = two.a;
                  var _v36 = A2($elm$core$Dict$get, b, vars);
                  if (_v36.$ === 1) {
                    return _Utils_Tuple2(A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars), $elm$core$Result$Ok(one));
                  } else {
                    var foundTwo = _v36.a;
                    var $temp$aliases = aliases, $temp$vars = vars, $temp$one = one, $temp$two = foundTwo;
                    aliases = $temp$aliases;
                    vars = $temp$vars;
                    one = $temp$one;
                    two = $temp$two;
                    continue unifiable;
                  }
                case 2:
                  return _Utils_Tuple2(vars, $elm$core$Result$Ok($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit));
                default:
                  return _Utils_Tuple2(vars, $elm$core$Result$Err(A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
              }
            case 3:
              var valsA = one.a;
              switch (two.$) {
                case 0:
                  var b = two.a;
                  var _v38 = A2($elm$core$Dict$get, b, vars);
                  if (_v38.$ === 1) {
                    return _Utils_Tuple2(A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars), $elm$core$Result$Ok(one));
                  } else {
                    var foundTwo = _v38.a;
                    var $temp$aliases = aliases, $temp$vars = vars, $temp$one = one, $temp$two = foundTwo;
                    aliases = $temp$aliases;
                    vars = $temp$vars;
                    one = $temp$one;
                    two = $temp$two;
                    continue unifiable;
                  }
                case 3:
                  var valsB = two.a;
                  var _v39 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableLists, aliases, vars, valsA, valsB, _List_Nil);
                  if (!_v39.b.$) {
                    var newVars = _v39.a;
                    var unified = _v39.b.a;
                    return _Utils_Tuple2(newVars, $elm$core$Result$Ok($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(unified)));
                  } else {
                    var newVars = _v39.a;
                    var err = _v39.b.a;
                    return _Utils_Tuple2(newVars, $elm$core$Result$Err(err));
                  }
                default:
                  return _Utils_Tuple2(vars, $elm$core$Result$Err(A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
              }
            case 4:
              var fieldsA = one.a;
              switch (two.$) {
                case 0:
                  var b = two.a;
                  var _v41 = A2($elm$core$Dict$get, b, vars);
                  if (_v41.$ === 1) {
                    return _Utils_Tuple2(A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars), $elm$core$Result$Ok(one));
                  } else {
                    var foundTwo = _v41.a;
                    var $temp$aliases = aliases, $temp$vars = vars, $temp$one = one, $temp$two = foundTwo;
                    aliases = $temp$aliases;
                    vars = $temp$vars;
                    one = $temp$one;
                    two = $temp$two;
                    continue unifiable;
                  }
                case 5:
                  var _v42 = two.a;
                  var twoRecName = _v42.b;
                  var _v43 = two.b;
                  var fieldsB = _v43.b;
                  var _v44 = A2($elm$core$Dict$get, twoRecName, vars);
                  if (_v44.$ === 1) {
                    var _v45 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
                    if (!_v45.b.$) {
                      var newVars = _v45.a;
                      var unifiedFields = _v45.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Ok($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
                    } else {
                      var newVars = _v45.a;
                      var err = _v45.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Err(err));
                    }
                  } else {
                    var _v46 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
                    if (!_v46.b.$) {
                      var newVars = _v46.a;
                      var unifiedFields = _v46.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Ok($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
                    } else {
                      var newVars = _v46.a;
                      var err = _v46.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Err(err));
                    }
                  }
                case 4:
                  var fieldsB = two.a;
                  var _v47 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
                  if (!_v47.b.$) {
                    var newVars = _v47.a;
                    var unifiedFields = _v47.b.a;
                    return _Utils_Tuple2(newVars, $elm$core$Result$Ok($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
                  } else {
                    var newVars = _v47.a;
                    var err = _v47.b.a;
                    return _Utils_Tuple2(newVars, $elm$core$Result$Err(err));
                  }
                case 1:
                  var twoName = two.a;
                  var twoVars = two.b;
                  var _v48 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifyWithAlias, aliases, vars, twoName, twoVars, one);
                  if (_v48.$ === 1) {
                    return _Utils_Tuple2(vars, $elm$core$Result$Err(A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
                  } else {
                    var unified = _v48.a;
                    return unified;
                  }
                default:
                  return _Utils_Tuple2(vars, $elm$core$Result$Err(A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
              }
            case 5:
              var _v49 = one.b;
              var fieldsA = _v49.b;
              switch (two.$) {
                case 0:
                  var b = two.a;
                  var _v51 = A2($elm$core$Dict$get, b, vars);
                  if (_v51.$ === 1) {
                    return _Utils_Tuple2(A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars), $elm$core$Result$Ok(one));
                  } else {
                    var foundTwo = _v51.a;
                    var $temp$aliases = aliases, $temp$vars = vars, $temp$one = one, $temp$two = foundTwo;
                    aliases = $temp$aliases;
                    vars = $temp$vars;
                    one = $temp$one;
                    two = $temp$two;
                    continue unifiable;
                  }
                case 5:
                  var _v52 = two.a;
                  var twoRecName = _v52.b;
                  var _v53 = two.b;
                  var fieldsB = _v53.b;
                  var _v54 = A2($elm$core$Dict$get, twoRecName, vars);
                  if (_v54.$ === 1) {
                    var _v55 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
                    if (!_v55.b.$) {
                      var newVars = _v55.a;
                      var unifiedFields = _v55.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Ok($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
                    } else {
                      var newVars = _v55.a;
                      var err = _v55.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Err(err));
                    }
                  } else {
                    var _v56 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
                    if (!_v56.b.$) {
                      var newVars = _v56.a;
                      var unifiedFields = _v56.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Ok($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
                    } else {
                      var newVars = _v56.a;
                      var err = _v56.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Err(err));
                    }
                  }
                case 4:
                  var fieldsB = two.a;
                  var _v57 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
                  if (!_v57.b.$) {
                    var newVars = _v57.a;
                    var unifiedFields = _v57.b.a;
                    return _Utils_Tuple2(newVars, $elm$core$Result$Ok($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
                  } else {
                    var newVars = _v57.a;
                    var err = _v57.b.a;
                    return _Utils_Tuple2(newVars, $elm$core$Result$Err(err));
                  }
                case 1:
                  var twoName = two.a;
                  var twoVars = two.b;
                  var _v58 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifyWithAlias, aliases, vars, twoName, twoVars, one);
                  if (_v58.$ === 1) {
                    return _Utils_Tuple2(vars, $elm$core$Result$Err(A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
                  } else {
                    var unified = _v58.a;
                    return unified;
                  }
                default:
                  return _Utils_Tuple2(vars, $elm$core$Result$Err(A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
              }
            default:
              var _v59 = one.a;
              var oneA = _v59.b;
              var _v60 = one.b;
              var oneB = _v60.b;
              switch (two.$) {
                case 0:
                  var b = two.a;
                  var _v62 = A2($elm$core$Dict$get, b, vars);
                  if (_v62.$ === 1) {
                    return _Utils_Tuple2(A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars), $elm$core$Result$Ok(one));
                  } else {
                    var foundTwo = _v62.a;
                    var $temp$aliases = aliases, $temp$vars = vars, $temp$one = one, $temp$two = foundTwo;
                    aliases = $temp$aliases;
                    vars = $temp$vars;
                    one = $temp$one;
                    two = $temp$two;
                    continue unifiable;
                  }
                case 6:
                  var _v63 = two.a;
                  var twoA = _v63.b;
                  var _v64 = two.b;
                  var twoB = _v64.b;
                  var _v65 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, aliases, vars, oneA, twoA);
                  if (!_v65.b.$) {
                    var aVars = _v65.a;
                    var unifiedA = _v65.b.a;
                    var _v66 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, aliases, aVars, oneB, twoB);
                    if (!_v66.b.$) {
                      var bVars = _v66.a;
                      var unifiedB = _v66.b.a;
                      return _Utils_Tuple2(bVars, $elm$core$Result$Ok(A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, $mdgriffith$elm_codegen$Internal$Compiler$nodify(unifiedA), $mdgriffith$elm_codegen$Internal$Compiler$nodify(unifiedB))));
                    } else {
                      var otherwise = _v66;
                      return otherwise;
                    }
                  } else {
                    var otherwise = _v65;
                    return otherwise;
                  }
                default:
                  return _Utils_Tuple2(vars, $elm$core$Result$Err(A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
              }
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$unifiableFields = F5(function(aliases, vars, one, two, unified) {
      unifiableFields:
        while (true) {
          var _v17 = _Utils_Tuple2(one, two);
          if (!_v17.a.b) {
            if (!_v17.b.b) {
              return _Utils_Tuple2(vars, $elm$core$Result$Ok($mdgriffith$elm_codegen$Internal$Compiler$nodifyAll($elm$core$List$reverse(unified))));
            } else {
              return _Utils_Tuple2(vars, $elm$core$Result$Err($mdgriffith$elm_codegen$Internal$Compiler$MismatchedTypeVariables));
            }
          } else {
            var _v18 = _v17.a;
            var _v19 = _v18.a;
            var _v20 = _v19.b;
            var _v21 = _v20.a;
            var oneFieldName = _v21.b;
            var _v22 = _v20.b;
            var oneFieldVal = _v22.b;
            var oneRemain = _v18.b;
            var twoFields = _v17.b;
            var _v23 = A4($mdgriffith$elm_codegen$Internal$Compiler$getField, oneFieldName, oneFieldVal, twoFields, _List_Nil);
            if (!_v23.$) {
              var _v24 = _v23.a;
              var matchingFieldVal = _v24.a;
              var remainingTwo = _v24.b;
              var _v25 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, aliases, vars, oneFieldVal, matchingFieldVal);
              var newVars = _v25.a;
              var unifiedFieldResult = _v25.b;
              if (!unifiedFieldResult.$) {
                var unifiedField = unifiedFieldResult.a;
                var $temp$aliases = aliases, $temp$vars = newVars, $temp$one = oneRemain, $temp$two = remainingTwo, $temp$unified = A2($elm$core$List$cons, _Utils_Tuple2($mdgriffith$elm_codegen$Internal$Compiler$nodify(oneFieldName), $mdgriffith$elm_codegen$Internal$Compiler$nodify(unifiedField)), unified);
                aliases = $temp$aliases;
                vars = $temp$vars;
                one = $temp$one;
                two = $temp$two;
                unified = $temp$unified;
                continue unifiableFields;
              } else {
                var err = unifiedFieldResult.a;
                return _Utils_Tuple2(newVars, $elm$core$Result$Err(err));
              }
            } else {
              var notFound = _v23.a;
              return _Utils_Tuple2(vars, $elm$core$Result$Err(notFound));
            }
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$unifiableLists = F5(function(aliases, vars, one, two, unified) {
      unifiableLists:
        while (true) {
          var _v6 = _Utils_Tuple2(one, two);
          _v6$3:
            while (true) {
              if (!_v6.a.b) {
                if (!_v6.b.b) {
                  return _Utils_Tuple2(vars, $elm$core$Result$Ok($mdgriffith$elm_codegen$Internal$Compiler$nodifyAll($elm$core$List$reverse(unified))));
                } else {
                  break _v6$3;
                }
              } else {
                if (_v6.b.b) {
                  if (!_v6.a.b.b && !_v6.b.b.b) {
                    var _v7 = _v6.a;
                    var _v8 = _v7.a;
                    var oneX = _v8.b;
                    var _v9 = _v6.b;
                    var _v10 = _v9.a;
                    var twoX = _v10.b;
                    var _v11 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, aliases, vars, oneX, twoX);
                    if (!_v11.b.$) {
                      var newVars = _v11.a;
                      var un = _v11.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Ok($mdgriffith$elm_codegen$Internal$Compiler$nodifyAll($elm$core$List$reverse(A2($elm$core$List$cons, un, unified)))));
                    } else {
                      var newVars = _v11.a;
                      var err = _v11.b.a;
                      return _Utils_Tuple2(newVars, $elm$core$Result$Err(err));
                    }
                  } else {
                    var _v12 = _v6.a;
                    var _v13 = _v12.a;
                    var oneX = _v13.b;
                    var oneRemain = _v12.b;
                    var _v14 = _v6.b;
                    var _v15 = _v14.a;
                    var twoX = _v15.b;
                    var twoRemain = _v14.b;
                    var _v16 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, aliases, vars, oneX, twoX);
                    if (!_v16.b.$) {
                      var newVars = _v16.a;
                      var un = _v16.b.a;
                      var $temp$aliases = aliases, $temp$vars = newVars, $temp$one = oneRemain, $temp$two = twoRemain, $temp$unified = A2($elm$core$List$cons, un, unified);
                      aliases = $temp$aliases;
                      vars = $temp$vars;
                      one = $temp$one;
                      two = $temp$two;
                      unified = $temp$unified;
                      continue unifiableLists;
                    } else {
                      var err = _v16.b.a;
                      return _Utils_Tuple2(vars, $elm$core$Result$Err(err));
                    }
                  }
                } else {
                  break _v6$3;
                }
              }
            }
          return _Utils_Tuple2(vars, $elm$core$Result$Err($mdgriffith$elm_codegen$Internal$Compiler$MismatchedTypeVariables));
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$unifyWithAlias = F5(function(aliases, vars, typename, typeVars, typeToUnifyWith) {
      var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$getAlias, typename, aliases);
      if (_v0.$ === 1) {
        return $elm$core$Maybe$Nothing;
      } else {
        var foundAlias = _v0.a;
        var fullAliasedType = function() {
          var _v3 = foundAlias.bs;
          if (!_v3.b) {
            return foundAlias.hf;
          } else {
            var makeAliasVarCache = F2(function(varName, _v5) {
              var varType = _v5.b;
              return _Utils_Tuple2(varName, varType);
            });
            var _v4 = A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, $elm$core$Set$empty, $elm$core$Dict$fromList(A3($elm$core$List$map2, makeAliasVarCache, foundAlias.bs, typeVars)), foundAlias.hf);
            if (!_v4.$) {
              var resolvedType = _v4.a;
              return resolvedType;
            } else {
              return foundAlias.hf;
            }
          }
        }();
        var _v1 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, aliases, vars, fullAliasedType, typeToUnifyWith);
        var returnedVars = _v1.a;
        var unifiedResult = _v1.b;
        if (!unifiedResult.$) {
          return $elm$core$Maybe$Just(_Utils_Tuple2(returnedVars, $elm$core$Result$Ok(fullAliasedType)));
        } else {
          return $elm$core$Maybe$Nothing;
        }
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$applyTypeHelper = F4(function(aliases, cache, fn, args) {
      applyTypeHelper:
        while (true) {
          switch (fn.$) {
            case 6:
              var _v1 = fn.a;
              var one = _v1.b;
              var _v2 = fn.b;
              var two = _v2.b;
              if (!args.b) {
                return $elm$core$Result$Ok({ ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: cache, hG: fn });
              } else {
                var top = args.a;
                var rest = args.b;
                var _v4 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, aliases, cache, one, top);
                if (!_v4.b.$) {
                  var variableCache = _v4.a;
                  var $temp$aliases = aliases, $temp$cache = variableCache, $temp$fn = two, $temp$args = rest;
                  aliases = $temp$aliases;
                  cache = $temp$cache;
                  fn = $temp$fn;
                  args = $temp$args;
                  continue applyTypeHelper;
                } else {
                  var err = _v4.b.a;
                  return $elm$core$Result$Err(_List_fromArray([err]));
                }
              }
            case 0:
              var varName = fn.a;
              if (!args.b) {
                return $elm$core$Result$Ok({ ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: cache, hG: fn });
              } else {
                var resultType = $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(varName + "_result");
                return $elm$core$Result$Ok({
                  ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
                  e: A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, varName, A2($mdgriffith$elm_codegen$Internal$Compiler$makeFunction, resultType, args), cache),
                  hG: resultType
                });
              }
            default:
              var _final = fn;
              if (!args.b) {
                return $elm$core$Result$Ok({ ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: cache, hG: fn });
              } else {
                return $elm$core$Result$Err(_List_fromArray([
                  A2($mdgriffith$elm_codegen$Internal$Compiler$FunctionAppliedToTooManyArgs, _final, args)
                ]));
              }
          }
        }
    });
    var $elm$core$Dict$merge = F6(function(leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
      var stepState = F3(function(rKey, rValue, _v0) {
        stepState:
          while (true) {
            var list = _v0.a;
            var result = _v0.b;
            if (!list.b) {
              return _Utils_Tuple2(list, A3(rightStep, rKey, rValue, result));
            } else {
              var _v2 = list.a;
              var lKey = _v2.a;
              var lValue = _v2.b;
              var rest = list.b;
              if (_Utils_cmp(lKey, rKey) < 0) {
                var $temp$rKey = rKey, $temp$rValue = rValue, $temp$_v0 = _Utils_Tuple2(rest, A3(leftStep, lKey, lValue, result));
                rKey = $temp$rKey;
                rValue = $temp$rValue;
                _v0 = $temp$_v0;
                continue stepState;
              } else {
                if (_Utils_cmp(lKey, rKey) > 0) {
                  return _Utils_Tuple2(list, A3(rightStep, rKey, rValue, result));
                } else {
                  return _Utils_Tuple2(rest, A4(bothStep, lKey, lValue, rValue, result));
                }
              }
            }
          }
      });
      var _v3 = A3($elm$core$Dict$foldl, stepState, _Utils_Tuple2($elm$core$Dict$toList(leftDict), initialResult), rightDict);
      var leftovers = _v3.a;
      var intermediateResult = _v3.b;
      return A3($elm$core$List$foldl, F2(function(_v4, result) {
        var k = _v4.a;
        var v = _v4.b;
        return A3(leftStep, k, v, result);
      }), intermediateResult, leftovers);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$mergeInferences = F2(function(one, two) {
      return A6($elm$core$Dict$merge, $elm$core$Dict$insert, F4(function(key, oneVal, twoVal, d) {
        if (oneVal.$ === 5) {
          var recordName = oneVal.a;
          var _v1 = oneVal.b;
          var oneRange = _v1.a;
          var recordDefinition = _v1.b;
          if (twoVal.$ === 5) {
            var _v3 = twoVal.b;
            var twoRecordDefinition = _v3.b;
            return A3($elm$core$Dict$insert, key, A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord, recordName, A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, oneRange, _Utils_ap(recordDefinition, twoRecordDefinition))), d);
          } else {
            return A3($elm$core$Dict$insert, key, oneVal, d);
          }
        } else {
          return A3($elm$core$Dict$insert, key, oneVal, d);
        }
      }), $elm$core$Dict$insert, one, two, $elm$core$Dict$empty);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$mergeArgInferences = F3(function(expressions, annotations, inferences) {
      mergeArgInferences:
        while (true) {
          if (!expressions.b) {
            return $elm$core$Result$Ok({
              e: inferences,
              al: $elm$core$List$reverse(annotations)
            });
          } else {
            var top = expressions.a;
            var remain = expressions.b;
            var _v1 = top.bM;
            if (!_v1.$) {
              var ann = _v1.a;
              var $temp$expressions = remain, $temp$annotations = A2($elm$core$List$cons, ann.hG, annotations), $temp$inferences = A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, inferences, ann.e);
              expressions = $temp$expressions;
              annotations = $temp$annotations;
              inferences = $temp$inferences;
              continue mergeArgInferences;
            } else {
              var err = _v1.a;
              return $elm$core$Result$Err(err);
            }
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$applyType = F3(function(index, annotation, args) {
      if (annotation.$ === 1) {
        var err = annotation.a;
        return $elm$core$Result$Err(err);
      } else {
        var fnAnnotation = annotation.a;
        if ($mdgriffith$elm_codegen$Internal$Index$typecheck(index)) {
          var _v1 = A3($mdgriffith$elm_codegen$Internal$Compiler$mergeArgInferences, args, _List_Nil, fnAnnotation.e);
          if (!_v1.$) {
            var mergedArgs = _v1.a;
            return A4($mdgriffith$elm_codegen$Internal$Compiler$applyTypeHelper, fnAnnotation.ib, mergedArgs.e, fnAnnotation.hG, mergedArgs.al);
          } else {
            var err = _v1.a;
            return $elm$core$Result$Err(err);
          }
        } else {
          return $elm$core$Result$Err(_List_Nil);
        }
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$Expression = $elm$core$Basics$identity;
    var $mdgriffith$elm_codegen$Internal$Index$dive = function(_v0) {
      var modName = _v0.a;
      var top = _v0.b;
      var tail = _v0.c;
      var scope2 = _v0.d;
      var check = _v0.e;
      return A5($mdgriffith$elm_codegen$Internal$Index$Index, modName, 0, A2($elm$core$List$cons, top, tail), scope2, check);
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$expression = function(toExp) {
      return function(index) {
        return toExp($mdgriffith$elm_codegen$Internal$Index$dive(index));
      };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$getImports = function(exp) {
      return exp.c;
    };
    var $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression = function(a) {
      return { $: 14, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$parens = function(expr) {
      switch (expr.$) {
        case 0:
          return expr;
        case 7:
          return expr;
        case 11:
          return expr;
        case 8:
          return expr;
        case 9:
          return expr;
        case 13:
          return expr;
        case 14:
          return expr;
        case 12:
          return expr;
        case 19:
          return expr;
        case 3:
          return expr;
        case 21:
          return expr;
        case 22:
          return expr;
        case 18:
          return expr;
        case 17:
          return expr;
        default:
          return $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression($mdgriffith$elm_codegen$Internal$Compiler$nodify(expr));
      }
    };
    var $mdgriffith$elm_codegen$Internal$Index$next = function(_v0) {
      var modName = _v0.a;
      var top = _v0.b;
      var tail = _v0.c;
      var scope2 = _v0.d;
      var check = _v0.e;
      return A5($mdgriffith$elm_codegen$Internal$Index$Index, modName, top + 1, tail, scope2, check);
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$threadHelper = F3(function(index, exps, rendered) {
      threadHelper:
        while (true) {
          if (!exps.b) {
            return $elm$core$List$reverse(rendered);
          } else {
            var toExpDetails = exps.a;
            var remain = exps.b;
            var $temp$index = $mdgriffith$elm_codegen$Internal$Index$next(index), $temp$exps = remain, $temp$rendered = A2($elm$core$List$cons, toExpDetails(index), rendered);
            index = $temp$index;
            exps = $temp$exps;
            rendered = $temp$rendered;
            continue threadHelper;
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$thread = F2(function(index, exps) {
      return A3($mdgriffith$elm_codegen$Internal$Compiler$threadHelper, index, exps, _List_Nil);
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails = F2(function(index, _v0) {
      var toExp = _v0;
      return _Utils_Tuple2($mdgriffith$elm_codegen$Internal$Index$next(index), toExp(index));
    });
    var $mdgriffith$elm_codegen$Elm$apply = F2(function(fnExp, argExpressions) {
      return $mdgriffith$elm_codegen$Internal$Compiler$expression(function(index) {
        var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, fnExp);
        var annotationIndex = _v0.a;
        var fnDetails = _v0.b;
        var args = A2($mdgriffith$elm_codegen$Internal$Compiler$thread, annotationIndex, argExpressions);
        return {
          bM: A3($mdgriffith$elm_codegen$Internal$Compiler$applyType, index, fnDetails.bM, args),
          iH: $stil4m$elm_syntax$Elm$Syntax$Expression$Application($mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(A2($elm$core$List$cons, fnDetails.iH, A2($elm$core$List$map, A2($elm$core$Basics$composeL, $mdgriffith$elm_codegen$Internal$Compiler$parens, function($) {
            return $.iH;
          }), args)))),
          c: _Utils_ap(fnDetails.c, A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getImports, args))
        };
      });
    });
    var $stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue = F2(function(a, b) {
      return { $: 3, a, b };
    });
    var $mdgriffith$elm_codegen$Internal$Types$nodify = function(exp) {
      return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, exp);
    };
    var $mdgriffith$elm_codegen$Internal$Types$bool = A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Types$nodify(_Utils_Tuple2(_List_Nil, "Bool")), _List_Nil);
    var $mdgriffith$elm_codegen$Elm$bool = function(on) {
      return function(_v0) {
        return {
          bM: $elm$core$Result$Ok({ ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: $elm$core$Dict$empty, hG: $mdgriffith$elm_codegen$Internal$Types$bool }),
          iH: A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, on ? "True" : "False"),
          c: _List_Nil
        };
      };
    };
    var $mdgriffith$elm_codegen$Internal$Branch$Branch = $elm$core$Basics$identity;
    var $mdgriffith$elm_codegen$Internal$Arg$toDetails = F2(function(index, _v0) {
      var arg = _v0;
      return arg(index);
    });
    var $mdgriffith$elm_codegen$Elm$Case$branch = F2(function(arg, toBody) {
      return function(index) {
        var argDetails = A2($mdgriffith$elm_codegen$Internal$Arg$toDetails, index, arg);
        return _Utils_Tuple3(argDetails.at, $mdgriffith$elm_codegen$Internal$Compiler$denode(argDetails.cQ.f$), toBody(argDetails.hU));
      };
    });
    var $author$project$Interactive$capitalize = function(str) {
      var top = A2($elm$core$String$left, 1, str);
      var remain = A2($elm$core$String$dropLeft, 1, str);
      return _Utils_ap($elm$core$String$toUpper(top), remain);
    };
    var $mdgriffith$elm_codegen$Internal$Arg$Arg = $elm$core$Basics$identity;
    var $stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern = F2(function(a, b) {
      return { $: 12, a, b };
    });
    var $mdgriffith$elm_codegen$Internal$Types$custom = F3(function(mod_, name, vars) {
      return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Types$nodify(_Utils_Tuple2(mod_, name)), A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Types$nodify, vars));
    });
    var $elm$core$List$isEmpty = function(xs) {
      if (!xs.b) {
        return true;
      } else {
        return false;
      }
    };
    var $mdgriffith$elm_codegen$Internal$Arg$customTypeWith = F2(function(details, toType) {
      var typeName = details.hF;
      var variantName = details.hX;
      return function(index) {
        var importFrom = A2($mdgriffith$elm_codegen$Internal$Index$getImport, index, details.eo);
        var imports = $elm$core$List$isEmpty(importFrom) ? _List_Nil : _List_fromArray([importFrom]);
        var annotation = $elm$core$Result$Ok({
          ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
          e: $elm$core$Dict$empty,
          hG: A3($mdgriffith$elm_codegen$Internal$Types$custom, importFrom, $mdgriffith$elm_codegen$Internal$Format$formatType(typeName), _List_Nil)
        });
        return {
          cQ: {
            bM: annotation,
            c: imports,
            f$: $mdgriffith$elm_codegen$Internal$Compiler$nodify(A2($stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern, {
              a3: importFrom,
              ag: $mdgriffith$elm_codegen$Internal$Format$formatType(variantName)
            }, _List_Nil))
          },
          at: $mdgriffith$elm_codegen$Internal$Index$dive(index),
          hU: toType
        };
      };
    });
    var $mdgriffith$elm_codegen$Internal$Arg$customType = F2(function(name, toType) {
      return A2($mdgriffith$elm_codegen$Internal$Arg$customTypeWith, {
        eo: _List_Nil,
        hF: $mdgriffith$elm_codegen$Internal$Format$formatValue(name),
        hX: name
      }, toType);
    });
    var $mdgriffith$elm_codegen$Elm$Arg$customType = $mdgriffith$elm_codegen$Internal$Arg$customType;
    var $mdgriffith$elm_codegen$Elm$Annotation$function = F2(function(anns, _return) {
      return {
        ib: A3($elm$core$List$foldl, F2(function(ann, aliases) {
          return A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, $mdgriffith$elm_codegen$Elm$Annotation$getAliases(ann), aliases);
        }), $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, A2($elm$core$List$cons, _return, anns)),
        bM: A3($elm$core$List$foldr, F2(function(ann, fn) {
          return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, $mdgriffith$elm_codegen$Internal$Compiler$nodify(ann), $mdgriffith$elm_codegen$Internal$Compiler$nodify(fn));
        }), $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(_return), A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation, anns)),
        c: _Utils_ap($mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(_return), A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, anns))
      };
    });
    var $mdgriffith$elm_codegen$Internal$Index$indexToString = function(_v0) {
      var top = _v0.b;
      var tail = _v0.c;
      return _Utils_ap(!top ? "" : "_" + $elm$core$String$fromInt(top), function() {
        if (!tail.b) {
          return "";
        } else {
          if (!tail.b.b) {
            var one = tail.a;
            return "_" + $elm$core$String$fromInt(one);
          } else {
            if (!tail.b.b.b) {
              var one = tail.a;
              var _v2 = tail.b;
              var two = _v2.a;
              return "_" + ($elm$core$String$fromInt(one) + ("_" + $elm$core$String$fromInt(two)));
            } else {
              if (!tail.b.b.b.b) {
                var one = tail.a;
                var _v3 = tail.b;
                var two = _v3.a;
                var _v4 = _v3.b;
                var three = _v4.a;
                return "_" + ($elm$core$String$fromInt(one) + ("_" + ($elm$core$String$fromInt(two) + ("_" + $elm$core$String$fromInt(three)))));
              } else {
                return "_" + A2($elm$core$String$join, "_", A2($elm$core$List$map, $elm$core$String$fromInt, tail));
              }
            }
          }
        }
      }());
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$mapNode = F2(function(fn, _v0) {
      var range = _v0.a;
      var n = _v0.b;
      return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, fn(n));
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation = F2(function(index, ann) {
      switch (ann.$) {
        case 0:
          var str = ann.a;
          return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(_Utils_ap(str, $mdgriffith$elm_codegen$Internal$Index$indexToString(index)));
        case 1:
          var modName = ann.a;
          var anns = ann.b;
          return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, modName, A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$mapNode($mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation(index)), anns));
        case 2:
          return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit;
        case 3:
          var tupled = ann.a;
          return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$mapNode($mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation(index)), tupled));
        case 4:
          var recordDefinition = ann.a;
          return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$protectField(index), recordDefinition));
        case 5:
          var recordName = ann.a;
          var _v3 = ann.b;
          var recordRange = _v3.a;
          var recordDefinition = _v3.b;
          return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord, A2($mdgriffith$elm_codegen$Internal$Compiler$mapNode, function(n) {
            return _Utils_ap(n, $mdgriffith$elm_codegen$Internal$Index$indexToString(index));
          }, recordName), A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, recordRange, A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$protectField(index), recordDefinition)));
        default:
          var one = ann.a;
          var two = ann.b;
          return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, A2($mdgriffith$elm_codegen$Internal$Compiler$mapNode, $mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation(index), one), A2($mdgriffith$elm_codegen$Internal$Compiler$mapNode, $mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation(index), two));
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$protectField = F2(function(index, _v0) {
      var nodeRange = _v0.a;
      var _v1 = _v0.b;
      var nodedName = _v1.a;
      var nodedType = _v1.b;
      return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, nodeRange, _Utils_Tuple2(nodedName, A2($mdgriffith$elm_codegen$Internal$Compiler$mapNode, $mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation(index), nodedType)));
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$getInnerInference = F2(function(index, _v0) {
      var details = _v0;
      return {
        ib: details.ib,
        e: $elm$core$Dict$empty,
        hG: A2($mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation, index, details.bM)
      };
    });
    var $mdgriffith$elm_codegen$Internal$Index$protectTypeName = F2(function(base, index) {
      var tail = index.c;
      if (!tail.b) {
        return $mdgriffith$elm_codegen$Internal$Format$formatValue(base);
      } else {
        return $mdgriffith$elm_codegen$Internal$Format$formatValue(_Utils_ap(base, $mdgriffith$elm_codegen$Internal$Index$indexToString(index)));
      }
    });
    var $mdgriffith$elm_codegen$Elm$value = function(details) {
      return function(index) {
        var importFrom = A2($mdgriffith$elm_codegen$Internal$Index$getImport, index, details.eo);
        return {
          bM: function() {
            var _v0 = details.bM;
            if (_v0.$ === 1) {
              var typename = A2($mdgriffith$elm_codegen$Internal$Index$protectTypeName, details.ag, index);
              return $elm$core$Result$Ok({
                ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
                e: $elm$core$Dict$empty,
                hG: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(typename)
              });
            } else {
              var ann = _v0.a;
              return $elm$core$Result$Ok(A2($mdgriffith$elm_codegen$Internal$Compiler$getInnerInference, index, ann));
            }
          }(),
          iH: A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, importFrom, $mdgriffith$elm_codegen$Internal$Format$sanitize(details.ag)),
          c: function() {
            var _v1 = details.bM;
            if (_v1.$ === 1) {
              if (!importFrom.b) {
                return _List_Nil;
              } else {
                return _List_fromArray([importFrom]);
              }
            } else {
              var ann = _v1.a;
              if (!importFrom.b) {
                return $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(ann);
              } else {
                return A2($elm$core$List$cons, importFrom, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(ann));
              }
            }
          }()
        };
      };
    };
    var $author$project$Gen$Json$Decode$decodeValue = F2(function(decodeValueArg_, decodeValueArg_0) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
          A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Decode"]), "Decoder", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ])),
          A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Decode"]), "Value", _List_Nil)
        ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Result"]), "Result", _List_fromArray([
          A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Decode"]), "Error", _List_Nil),
          $mdgriffith$elm_codegen$Elm$Annotation$var("a")
        ])))),
        eo: _List_fromArray(["Json", "Decode"]),
        ag: "decodeValue"
      }), _List_fromArray([decodeValueArg_, decodeValueArg_0]));
    });
    var $mdgriffith$elm_codegen$Elm$Annotation$typed = F3(function(mod, name, args) {
      return {
        ib: A3($elm$core$List$foldl, F2(function(ann, aliases) {
          return A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, $mdgriffith$elm_codegen$Elm$Annotation$getAliases(ann), aliases);
        }), $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, args),
        bM: A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2(mod, name)), $mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation, args))),
        c: A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, args)
      };
    });
    var $mdgriffith$elm_codegen$Elm$Annotation$bool = A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_Nil, "Bool", _List_Nil);
    var $author$project$Gen$Json$Decode$bool = $mdgriffith$elm_codegen$Elm$value({
      bM: $elm$core$Maybe$Just(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Decode"]), "Decoder", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$bool]))),
      eo: _List_fromArray(["Json", "Decode"]),
      ag: "bool"
    });
    var $mdgriffith$elm_codegen$Elm$Annotation$float = A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_Nil, "Float", _List_Nil);
    var $author$project$Gen$Json$Decode$float = $mdgriffith$elm_codegen$Elm$value({
      bM: $elm$core$Maybe$Just(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Decode"]), "Decoder", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$float]))),
      eo: _List_fromArray(["Json", "Decode"]),
      ag: "float"
    });
    var $mdgriffith$elm_codegen$Elm$Annotation$int = A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_Nil, "Int", _List_Nil);
    var $author$project$Gen$Json$Decode$int = $mdgriffith$elm_codegen$Elm$value({
      bM: $elm$core$Maybe$Just(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Decode"]), "Decoder", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$int]))),
      eo: _List_fromArray(["Json", "Decode"]),
      ag: "int"
    });
    var $mdgriffith$elm_codegen$Elm$Annotation$maybe = function(maybeArg) {
      return A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_Nil, "Maybe", _List_fromArray([maybeArg]));
    };
    var $author$project$Gen$Json$Decode$maybe = function(maybeArg_) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
          A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Decode"]), "Decoder", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ]))
        ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Decode"]), "Decoder", _List_fromArray([
          $mdgriffith$elm_codegen$Elm$Annotation$maybe($mdgriffith$elm_codegen$Elm$Annotation$var("a"))
        ])))),
        eo: _List_fromArray(["Json", "Decode"]),
        ag: "maybe"
      }), _List_fromArray([maybeArg_]));
    };
    var $mdgriffith$elm_codegen$Elm$Annotation$string = A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_Nil, "String", _List_Nil);
    var $author$project$Gen$Json$Decode$string = $mdgriffith$elm_codegen$Elm$value({
      bM: $elm$core$Maybe$Just(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Decode"]), "Decoder", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]))),
      eo: _List_fromArray(["Json", "Decode"]),
      ag: "string"
    });
    var $author$project$Interactive$toDecoderHelper = function(input) {
      switch (input.$) {
        case 0:
          return $author$project$Gen$Json$Decode$string;
        case 1:
          return $author$project$Gen$Json$Decode$bool;
        case 2:
          return $author$project$Gen$Json$Decode$int;
        case 3:
          return $author$project$Gen$Json$Decode$float;
        default:
          var inner = input.a;
          return $author$project$Gen$Json$Decode$maybe($author$project$Interactive$toDecoderHelper(inner));
      }
    };
    var $author$project$Interactive$decode = F2(function(input, val) {
      return A2($author$project$Gen$Json$Decode$decodeValue, $author$project$Interactive$toDecoderHelper(input), val);
    });
    var $mdgriffith$elm_codegen$Elm$deduplicate = function(listToDeduplicate) {
      return $elm$core$List$reverse(A3($elm$core$List$foldl, F2(function(item, untouched) {
        var set = untouched.a;
        var innerList = untouched.b;
        return A2($elm$core$Set$member, item, set) ? untouched : _Utils_Tuple2(A2($elm$core$Set$insert, item, set), A2($elm$core$List$cons, item, innerList));
      }), _Utils_Tuple2($elm$core$Set$empty, _List_Nil), listToDeduplicate).b);
    };
    var $mdgriffith$elm_codegen$Elm$getGenerics = function(_v0) {
      var keepExtra = _v0.a0;
      var requested = _v0.bd;
      var needed = _v0.a4;
      var requestedList = keepExtra ? $elm$core$List$reverse($mdgriffith$elm_codegen$Elm$deduplicate(requested)) : A3($elm$core$List$foldl, F2(function(generic, acc) {
        return A2($elm$core$List$member, generic, needed) ? A2($elm$core$List$cons, generic, acc) : acc;
      }), _List_Nil, $mdgriffith$elm_codegen$Elm$deduplicate(requested));
      return A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$nodify, $elm$core$List$reverse(A3($elm$core$List$foldl, F2(function(generic, acc) {
        return A2($elm$core$List$member, generic, requestedList) ? acc : A2($elm$core$List$cons, generic, acc);
      }), requestedList, $mdgriffith$elm_codegen$Elm$deduplicate(needed))));
    };
    var $mdgriffith$elm_codegen$Elm$aliasWith = F3(function(name, generics, innerAnnotation) {
      return $mdgriffith$elm_codegen$Internal$Compiler$Declaration({
        aQ: $elm$core$Maybe$Nothing,
        du: $mdgriffith$elm_codegen$Internal$Compiler$NotExposed,
        c: $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(innerAnnotation),
        ag: name,
        _: function(_v0) {
          return {
            X: _List_Nil,
            iw: $stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration({
              aR: $elm$core$Maybe$Nothing,
              dZ: $mdgriffith$elm_codegen$Elm$getGenerics({
                a0: false,
                a4: $mdgriffith$elm_codegen$Internal$Compiler$getGenerics(innerAnnotation),
                bd: generics
              }),
              ag: $mdgriffith$elm_codegen$Internal$Compiler$nodify($mdgriffith$elm_codegen$Internal$Format$formatType(name)),
              aH: $mdgriffith$elm_codegen$Internal$Compiler$nodify($mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(innerAnnotation))
            }),
            jE: $elm$core$Maybe$Nothing
          };
        }
      });
    });
    var $mdgriffith$elm_codegen$Elm$alias = F2(function(name, innerAnnotation) {
      return A3($mdgriffith$elm_codegen$Elm$aliasWith, name, _List_Nil, innerAnnotation);
    });
    var $author$project$Gen$Json$Decode$moduleName_ = _List_fromArray(["Json", "Decode"]);
    var $author$project$Gen$Json$Decode$annotation_ = {
      iy: function(decoderArg0) {
        return A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Decode"]), "Decoder", _List_fromArray([decoderArg0]));
      },
      dc: A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Decode"]), "Error", _List_Nil),
      hU: A4($mdgriffith$elm_codegen$Elm$Annotation$alias, $author$project$Gen$Json$Decode$moduleName_, "Value", _List_Nil, A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))
    };
    var $mdgriffith$elm_codegen$Elm$Annotation$record = function(fields) {
      return {
        ib: A3($elm$core$List$foldl, F2(function(_v0, aliases) {
          var ann = _v0.b;
          return A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, $mdgriffith$elm_codegen$Elm$Annotation$getAliases(ann), aliases);
        }), $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, fields),
        bM: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record($mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(A2($elm$core$List$map, function(_v1) {
          var name = _v1.a;
          var ann = _v1.b;
          return _Utils_Tuple2($mdgriffith$elm_codegen$Internal$Compiler$nodify($mdgriffith$elm_codegen$Internal$Format$formatValue(name)), $mdgriffith$elm_codegen$Internal$Compiler$nodify($mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(ann)));
        }, fields))),
        c: A2($elm$core$List$concatMap, A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports), fields)
      };
    };
    var $mdgriffith$elm_codegen$Elm$Annotation$tuple = F2(function(one, two) {
      return {
        ib: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, $mdgriffith$elm_codegen$Elm$Annotation$getAliases(one), $mdgriffith$elm_codegen$Elm$Annotation$getAliases(two)),
        bM: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled($mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(_List_fromArray([
          $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(one),
          $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(two)
        ]))),
        c: _Utils_ap($mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(one), $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(two))
      };
    });
    var $mdgriffith$elm_codegen$Elm$Annotation$unit = { ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, bM: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit, c: _List_Nil };
    var $author$project$Gen$Browser$call_ = {
      bP: function(applicationArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("init", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("flags"),
                A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Url"]), "Url", _List_Nil),
                A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Browser", "Navigation"]), "Key", _List_Nil)
              ]), A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$var("model"), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_Nil, "Cmd", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ]))))),
              _Utils_Tuple2("view", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("model")
              ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Browser"]), "Document", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ])))),
              _Utils_Tuple2("update", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg"),
                $mdgriffith$elm_codegen$Elm$Annotation$var("model")
              ]), A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$var("model"), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_Nil, "Cmd", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ]))))),
              _Utils_Tuple2("subscriptions", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("model")
              ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_Nil, "Sub", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ])))),
              _Utils_Tuple2("onUrlRequest", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Browser"]), "UrlRequest", _List_Nil)
              ]), $mdgriffith$elm_codegen$Elm$Annotation$var("msg"))),
              _Utils_Tuple2("onUrlChange", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Url"]), "Url", _List_Nil)
              ]), $mdgriffith$elm_codegen$Elm$Annotation$var("msg")))
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Platform"]), "Program", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("flags"),
            $mdgriffith$elm_codegen$Elm$Annotation$var("model"),
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Browser"]),
          ag: "application"
        }), _List_fromArray([applicationArg_]));
      },
      aq: function(documentArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("init", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("flags")
              ]), A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$var("model"), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_Nil, "Cmd", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ]))))),
              _Utils_Tuple2("view", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("model")
              ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Browser"]), "Document", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ])))),
              _Utils_Tuple2("update", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg"),
                $mdgriffith$elm_codegen$Elm$Annotation$var("model")
              ]), A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$var("model"), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_Nil, "Cmd", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ]))))),
              _Utils_Tuple2("subscriptions", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("model")
              ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_Nil, "Sub", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ]))))
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Platform"]), "Program", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("flags"),
            $mdgriffith$elm_codegen$Elm$Annotation$var("model"),
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Browser"]),
          ag: "document"
        }), _List_fromArray([documentArg_]));
      },
      c5: function(elementArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("init", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("flags")
              ]), A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$var("model"), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_Nil, "Cmd", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ]))))),
              _Utils_Tuple2("view", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("model")
              ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ])))),
              _Utils_Tuple2("update", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg"),
                $mdgriffith$elm_codegen$Elm$Annotation$var("model")
              ]), A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$var("model"), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_Nil, "Cmd", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ]))))),
              _Utils_Tuple2("subscriptions", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("model")
              ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_Nil, "Sub", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ]))))
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Platform"]), "Program", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("flags"),
            $mdgriffith$elm_codegen$Elm$Annotation$var("model"),
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Browser"]),
          ag: "element"
        }), _List_fromArray([elementArg_]));
      },
      gF: function(sandboxArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("init", $mdgriffith$elm_codegen$Elm$Annotation$var("model")),
              _Utils_Tuple2("view", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("model")
              ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
              ])))),
              _Utils_Tuple2("update", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("msg"),
                $mdgriffith$elm_codegen$Elm$Annotation$var("model")
              ]), $mdgriffith$elm_codegen$Elm$Annotation$var("model")))
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Platform"]), "Program", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$unit,
            $mdgriffith$elm_codegen$Elm$Annotation$var("model"),
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Browser"]),
          ag: "sandbox"
        }), _List_fromArray([sandboxArg_]));
      }
    };
    var $mdgriffith$elm_codegen$Elm$Annotation$named = F2(function(mod, name) {
      return {
        ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
        bM: A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2(mod, $mdgriffith$elm_codegen$Internal$Format$formatType(name))), _List_Nil),
        c: function() {
          if (!mod.b) {
            return _List_Nil;
          } else {
            return _List_fromArray([mod]);
          }
        }()
      };
    });
    var $author$project$GenApp$modelType = A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, "Model");
    var $author$project$GenApp$msgType = A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, "Msg");
    var $mdgriffith$elm_codegen$Internal$Compiler$DuplicateFieldInRecord = function(a) {
      return { $: 5, a };
    };
    var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr = function(a) {
      return { $: 18, a };
    };
    var $mdgriffith$elm_codegen$Elm$record = function(fields) {
      return $mdgriffith$elm_codegen$Internal$Compiler$expression(function(index) {
        var unified = A3($elm$core$List$foldl, F2(function(_v4, found) {
          var unformattedFieldName = _v4.a;
          var fieldExpression = _v4.b;
          var fieldName = $mdgriffith$elm_codegen$Internal$Format$formatValue(unformattedFieldName);
          var _v5 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, found.at, fieldExpression);
          var newIndex = _v5.a;
          var exp = _v5.b;
          return {
            N: function() {
              if (A2($elm$core$Set$member, fieldName, found.aA)) {
                return A2($elm$core$List$cons, $mdgriffith$elm_codegen$Internal$Compiler$DuplicateFieldInRecord(fieldName), found.N);
              } else {
                var _v6 = exp.bM;
                if (_v6.$ === 1) {
                  if (!_v6.a.b) {
                    return found.N;
                  } else {
                    var errs = _v6.a;
                    return _Utils_ap(errs, found.N);
                  }
                } else {
                  return found.N;
                }
              }
            }(),
            O: function() {
              var _v7 = exp.bM;
              if (_v7.$ === 1) {
                return found.O;
              } else {
                var ann = _v7.a;
                return A2($elm$core$List$cons, _Utils_Tuple2($mdgriffith$elm_codegen$Internal$Format$formatValue(fieldName), ann), found.O);
              }
            }(),
            aU: A2($elm$core$List$cons, _Utils_Tuple2($mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName), $mdgriffith$elm_codegen$Internal$Compiler$nodify(exp.iH)), found.aU),
            c: _Utils_ap(exp.c, found.c),
            at: newIndex,
            aA: A2($elm$core$Set$insert, fieldName, found.aA)
          };
        }), { N: _List_Nil, O: _List_Nil, aU: _List_Nil, c: _List_Nil, at: index, aA: $elm$core$Set$empty }, fields);
        return {
          bM: function() {
            var _v0 = unified.N;
            if (!_v0.b) {
              return $elm$core$Result$Ok({
                ib: A3($elm$core$List$foldl, F2(function(_v1, gathered) {
                  var ann = _v1.b;
                  return A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, ann.ib, gathered);
                }), $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, unified.O),
                e: A3($elm$core$List$foldl, F2(function(_v2, gathered) {
                  var ann = _v2.b;
                  return A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, ann.e, gathered);
                }), $elm$core$Dict$empty, unified.O),
                hG: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record($mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(A2($elm$core$List$map, function(_v3) {
                  var name = _v3.a;
                  var ann = _v3.b;
                  return _Utils_Tuple2($mdgriffith$elm_codegen$Internal$Compiler$nodify(name), $mdgriffith$elm_codegen$Internal$Compiler$nodify(ann.hG));
                }, $elm$core$List$reverse(unified.O))))
              });
            } else {
              var errs = _v0;
              return $elm$core$Result$Err(errs);
            }
          }(),
          iH: $stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr($mdgriffith$elm_codegen$Internal$Compiler$nodifyAll($elm$core$List$reverse(unified.aU))),
          c: unified.c
        };
      });
    };
    var $author$project$Gen$Platform$Cmd$annotation_ = {
      $9: function(cmdArg0) {
        return A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Cmd"]), "Cmd", _List_fromArray([cmdArg0]));
      }
    };
    var $author$project$GenApp$updateTupleType = A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $author$project$GenApp$modelType, $author$project$Gen$Platform$Cmd$annotation_.$9($author$project$GenApp$msgType));
    var $mdgriffith$elm_codegen$Elm$val = function(name) {
      return $mdgriffith$elm_codegen$Elm$value({ bM: $elm$core$Maybe$Nothing, eo: _List_Nil, ag: name });
    };
    var $author$project$GenApp$appMain = $author$project$Gen$Browser$call_.c5($mdgriffith$elm_codegen$Elm$record(_List_fromArray([
      _Utils_Tuple2("init", $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$author$project$Gen$Json$Decode$annotation_.hU]), $author$project$GenApp$updateTupleType)),
        eo: _List_Nil,
        ag: "init"
      })),
      _Utils_Tuple2("update", $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$author$project$GenApp$msgType, $author$project$GenApp$modelType]), $author$project$GenApp$updateTupleType)),
        eo: _List_Nil,
        ag: "update"
      })),
      _Utils_Tuple2("view", $mdgriffith$elm_codegen$Elm$val("view")),
      _Utils_Tuple2("subscriptions", $mdgriffith$elm_codegen$Elm$val("subscriptions"))
    ])));
    var $stil4m$elm_syntax$Elm$Syntax$Expression$CaseExpression = function(a) {
      return { $: 16, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$EmptyCaseStatement = { $: 2 };
    var $mdgriffith$elm_codegen$Elm$Case$combineInferences = F2(function(infs, infResult) {
      if (!infResult.$) {
        var inferred = infResult.a;
        return $elm$core$Result$Ok(_Utils_update(inferred, {
          e: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, infs, inferred.e)
        }));
      } else {
        var err = infResult.a;
        return $elm$core$Result$Err(err);
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$unifyOn = F2(function(_v0, res) {
      var annDetails = _v0;
      if (res.$ === 1) {
        return res;
      } else {
        var inf = res.a;
        var _v2 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, inf.ib, inf.e, annDetails.bM, inf.hG);
        var newInferences = _v2.a;
        var finalResult = _v2.b;
        if (!finalResult.$) {
          var finalType = finalResult.a;
          return $elm$core$Result$Ok({
            ib: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, annDetails.ib, inf.ib),
            e: newInferences,
            hG: finalType
          });
        } else {
          var err = finalResult.a;
          return $elm$core$Result$Err(_List_fromArray([err]));
        }
      }
    });
    var $mdgriffith$elm_codegen$Elm$Case$captureCaseHelper = F3(function(mainCaseExpressionModule, _v0, accum) {
      var toBranch = _v0;
      var _v1 = toBranch($mdgriffith$elm_codegen$Internal$Index$dive(accum.at));
      var branchIndex = _v1.a;
      var originalPattern = _v1.b;
      var caseExpression = _v1.c;
      var _v2 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, branchIndex, caseExpression);
      var exp = _v2.b;
      var pattern = function() {
        if (!mainCaseExpressionModule.b) {
          return originalPattern;
        } else {
          if (originalPattern.$ === 12) {
            var named = originalPattern.a;
            var vars = originalPattern.b;
            return A2($stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern, { a3: mainCaseExpressionModule, ag: named.ag }, vars);
          } else {
            return originalPattern;
          }
        }
      }();
      return {
        bM: function() {
          var _v3 = accum.bM;
          if (_v3.$ === 1) {
            return $elm$core$Maybe$Just(exp.bM);
          } else {
            if (!_v3.a.$) {
              var gatheredAnnotation = _v3.a.a;
              return $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Internal$Compiler$unifyOn, { ib: gatheredAnnotation.ib, bM: gatheredAnnotation.hG, c: _List_Nil }, A2($mdgriffith$elm_codegen$Elm$Case$combineInferences, gatheredAnnotation.e, exp.bM)));
            } else {
              var err = _v3.a;
              return $elm$core$Maybe$Just(err);
            }
          }
        }(),
        il: A2($elm$core$List$cons, _Utils_Tuple2($mdgriffith$elm_codegen$Internal$Compiler$nodify(pattern), $mdgriffith$elm_codegen$Internal$Compiler$nodify(exp.iH)), accum.il),
        c: _Utils_ap(accum.c, exp.c),
        at: $mdgriffith$elm_codegen$Internal$Index$next(accum.at)
      };
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$importInferences = F2(function(one, two) {
      return {
        ib: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, one.ib, two.ib),
        e: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, one.e, two.e),
        hG: two.hG
      };
    });
    var $mdgriffith$elm_codegen$Elm$Case$captureCase = F4(function(mainExpression, mainExpressionTypeModule, index, branches) {
      var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, mainExpression);
      var branchIndex = _v0.a;
      var mainExpressionDetails = _v0.b;
      var caseExp = A3($elm$core$List$foldl, $mdgriffith$elm_codegen$Elm$Case$captureCaseHelper(mainExpressionTypeModule), { bM: $elm$core$Maybe$Nothing, il: _List_Nil, c: _List_Nil, at: branchIndex }, branches);
      return _Utils_Tuple2(mainExpressionDetails, _Utils_update(caseExp, {
        bM: function() {
          var _v1 = caseExp.bM;
          if (!_v1.$ && !_v1.a.$) {
            var inference = _v1.a.a;
            var _v2 = mainExpressionDetails.bM;
            if (_v2.$ === 1) {
              var err = _v2.a;
              return $elm$core$Maybe$Just($elm$core$Result$Err(err));
            } else {
              var mainAnn = _v2.a;
              return $elm$core$Maybe$Just($elm$core$Result$Ok(A2($mdgriffith$elm_codegen$Internal$Compiler$importInferences, mainAnn, inference)));
            }
          } else {
            return caseExp.bM;
          }
        }()
      }));
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$getTypeModule = function(_v0) {
      var annotation = _v0;
      var _v1 = annotation.bM;
      if (_v1.$ === 1) {
        var _v2 = _v1.a;
        var _v3 = _v2.b;
        var mod = _v3.a;
        return mod;
      } else {
        return _List_Nil;
      }
    };
    var $mdgriffith$elm_codegen$Elm$withType = F2(function(ann, _v0) {
      var annDetails = ann;
      var toExp = _v0;
      return function(index) {
        var exp = toExp(index);
        return _Utils_update(exp, {
          bM: function() {
            var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$unifyOn, ann, exp.bM);
            if (!_v1.$) {
              var unified = _v1.a;
              return $elm$core$Result$Ok(unified);
            } else {
              var _v2 = exp.bM;
              if (!_v2.$) {
                var expressionAnnotation = _v2.a;
                return $elm$core$Result$Ok({ ib: expressionAnnotation.ib, e: expressionAnnotation.e, hG: annDetails.bM });
              } else {
                return $elm$core$Result$Ok({ ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: $elm$core$Dict$empty, hG: annDetails.bM });
              }
            }
          }(),
          c: _Utils_ap(exp.c, annDetails.c)
        });
      };
    });
    var $mdgriffith$elm_codegen$Elm$Case$custom = F3(function(mainExpression, annotation, branches) {
      return function(index) {
        var myMain = A2($mdgriffith$elm_codegen$Elm$withType, annotation, mainExpression);
        var _v0 = A4($mdgriffith$elm_codegen$Elm$Case$captureCase, myMain, $mdgriffith$elm_codegen$Internal$Compiler$getTypeModule(annotation), $mdgriffith$elm_codegen$Internal$Index$dive(index), branches);
        var expr = _v0.a;
        var gathered = _v0.b;
        return {
          bM: function() {
            var _v1 = gathered.bM;
            if (_v1.$ === 1) {
              return $elm$core$Result$Err(_List_fromArray([$mdgriffith$elm_codegen$Internal$Compiler$EmptyCaseStatement]));
            } else {
              var ann = _v1.a;
              return ann;
            }
          }(),
          iH: $stil4m$elm_syntax$Elm$Syntax$Expression$CaseExpression({
            il: $elm$core$List$reverse(gathered.il),
            iH: $mdgriffith$elm_codegen$Internal$Compiler$nodify(expr.iH)
          }),
          c: _Utils_ap(expr.c, gathered.c)
        };
      };
    });
    var $mdgriffith$elm_codegen$Elm$customTypeWith = F3(function(name, generics, variants) {
      return $mdgriffith$elm_codegen$Internal$Compiler$Declaration({
        aQ: $elm$core$Maybe$Nothing,
        du: $mdgriffith$elm_codegen$Internal$Compiler$NotExposed,
        c: A2($elm$core$List$concatMap, function(_v0) {
          var listAnn = _v0.b;
          return A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, listAnn);
        }, variants),
        ag: name,
        _: function(_v1) {
          return {
            X: _List_Nil,
            iw: $stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration({
              it: A2($elm$core$List$map, function(_v2) {
                var varName = _v2.a;
                var vars = _v2.b;
                return $mdgriffith$elm_codegen$Internal$Compiler$nodify({
                  aL: A2($elm$core$List$map, A2($elm$core$Basics$composeR, $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation, $mdgriffith$elm_codegen$Internal$Compiler$nodify), vars),
                  ag: $mdgriffith$elm_codegen$Internal$Compiler$nodify($mdgriffith$elm_codegen$Internal$Format$formatType(varName))
                });
              }, variants),
              aR: $elm$core$Maybe$Nothing,
              dZ: $mdgriffith$elm_codegen$Elm$getGenerics({
                a0: true,
                a4: A2($elm$core$List$concatMap, function(_v3) {
                  var listAnn = _v3.b;
                  return A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getGenerics, listAnn);
                }, variants),
                bd: generics
              }),
              ag: $mdgriffith$elm_codegen$Internal$Compiler$nodify($mdgriffith$elm_codegen$Internal$Format$formatType(name))
            }),
            jE: $elm$core$Maybe$Nothing
          };
        }
      });
    });
    var $mdgriffith$elm_codegen$Elm$customType = F2(function(name, variants) {
      return A3($mdgriffith$elm_codegen$Elm$customTypeWith, name, _List_Nil, variants);
    });
    var $mdgriffith$elm_codegen$Elm$Fn = $elm$core$Basics$identity;
    var $mdgriffith$elm_codegen$Elm$fnArg = F2(function(argument, _v0) {
      var toFnDetails = _v0;
      return function(index) {
        var argDetails = A2($mdgriffith$elm_codegen$Internal$Arg$toDetails, index, argument);
        var fnDetails = toFnDetails(argDetails.at);
        return {
          G: _Utils_ap(fnDetails.G, _List_fromArray([argDetails.cQ])),
          ih: fnDetails.ih(argDetails.hU),
          c: _Utils_ap(fnDetails.c, argDetails.cQ.c)
        };
      };
    });
    var $mdgriffith$elm_codegen$Elm$fnBuilder = function(innerValue) {
      return function(_v0) {
        return { G: _List_Nil, ih: innerValue, c: _List_Nil };
      };
    };
    var $stil4m$elm_syntax$Elm$Syntax$Expression$LambdaExpression = function(a) {
      return { $: 17, a };
    };
    var $mdgriffith$elm_codegen$Elm$fnDone = function(_v0) {
      var toFnDetails = _v0;
      return function(index) {
        var fnDetails = toFnDetails(index);
        var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, $mdgriffith$elm_codegen$Internal$Index$next(index), fnDetails.ih);
        var _return = _v1.b;
        return {
          bM: function() {
            var _v2 = _return.bM;
            if (_v2.$ === 1) {
              return _return.bM;
            } else {
              return A3($elm$core$List$foldr, F2(function(argDetails, result) {
                if (result.$ === 1) {
                  var err = result.a;
                  return $elm$core$Result$Err(err);
                } else {
                  var resultAnnotation = result.a;
                  var _v4 = argDetails.bM;
                  if (_v4.$ === 1) {
                    var err = _v4.a;
                    return $elm$core$Result$Err(err);
                  } else {
                    var argAnnotation = _v4.a;
                    return $elm$core$Result$Ok({
                      ib: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, resultAnnotation.ib, argAnnotation.ib),
                      e: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, resultAnnotation.e, argAnnotation.e),
                      hG: A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, $mdgriffith$elm_codegen$Internal$Compiler$nodify(argAnnotation.hG), $mdgriffith$elm_codegen$Internal$Compiler$nodify(resultAnnotation.hG))
                    });
                  }
                }
              }), _return.bM, fnDetails.G);
            }
          }(),
          iH: $stil4m$elm_syntax$Elm$Syntax$Expression$LambdaExpression({
            G: A2($elm$core$List$map, function($) {
              return $.f$;
            }, fnDetails.G),
            iH: $mdgriffith$elm_codegen$Internal$Compiler$nodify(_return.iH)
          }),
          c: _Utils_ap(fnDetails.c, _return.c)
        };
      };
    };
    var $mdgriffith$elm_codegen$Elm$fn = F2(function(arg1, toExpression) {
      return $mdgriffith$elm_codegen$Elm$fnDone(A2($mdgriffith$elm_codegen$Elm$fnArg, arg1, $mdgriffith$elm_codegen$Elm$fnBuilder(toExpression)));
    });
    var $mdgriffith$elm_codegen$Elm$fn2 = F3(function(arg1, arg2, toExpression) {
      return $mdgriffith$elm_codegen$Elm$fnDone(A2($mdgriffith$elm_codegen$Elm$fnArg, arg2, A2($mdgriffith$elm_codegen$Elm$fnArg, arg1, $mdgriffith$elm_codegen$Elm$fnBuilder(toExpression))));
    });
    var $stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern = function(a) {
      return { $: 11, a };
    };
    var $mdgriffith$elm_codegen$Internal$Index$getName = F2(function(desiredName, index) {
      var modName = index.a;
      var top = index.b;
      var tail = index.c;
      var scope2 = index.d;
      var check = index.e;
      var formattedName = $mdgriffith$elm_codegen$Internal$Format$formatValue(desiredName);
      if (!A2($elm$core$Set$member, formattedName, scope2)) {
        return _Utils_Tuple2(formattedName, A5($mdgriffith$elm_codegen$Internal$Index$Index, modName, top, tail, A2($elm$core$Set$insert, formattedName, scope2), check));
      } else {
        var protectedName = _Utils_ap(formattedName, $elm$core$String$fromInt(top));
        if (!A2($elm$core$Set$member, protectedName, scope2)) {
          return _Utils_Tuple2(protectedName, A5($mdgriffith$elm_codegen$Internal$Index$Index, modName, top + 1, tail, A2($elm$core$Set$insert, protectedName, scope2), check));
        } else {
          var protectedNameLevel2 = _Utils_ap(formattedName, $mdgriffith$elm_codegen$Internal$Index$indexToString(index));
          return _Utils_Tuple2(protectedNameLevel2, A5($mdgriffith$elm_codegen$Internal$Index$Index, modName, top + 1, tail, A2($elm$core$Set$insert, protectedNameLevel2, scope2), check));
        }
      }
    });
    var $mdgriffith$elm_codegen$Internal$Arg$varWith = F2(function(rawName, ann) {
      return function(index) {
        var imports = $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(ann);
        var _v0 = A2($mdgriffith$elm_codegen$Internal$Index$getName, rawName, index);
        var name = _v0.a;
        var nameIndex = _v0.b;
        var annotation = $elm$core$Result$Ok(A2($mdgriffith$elm_codegen$Internal$Compiler$getInnerInference, nameIndex, ann));
        return {
          cQ: {
            bM: annotation,
            c: imports,
            f$: $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(name))
          },
          at: $mdgriffith$elm_codegen$Internal$Index$next(nameIndex),
          hU: function(_v1) {
            return {
              bM: annotation,
              iH: A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, $mdgriffith$elm_codegen$Internal$Format$sanitize(name)),
              c: imports
            };
          }
        };
      };
    });
    var $mdgriffith$elm_codegen$Elm$Arg$varWith = $mdgriffith$elm_codegen$Internal$Arg$varWith;
    var $mdgriffith$elm_codegen$Elm$Variant = F2(function(a, b) {
      return { $: 0, a, b };
    });
    var $mdgriffith$elm_codegen$Elm$variantWith = $mdgriffith$elm_codegen$Elm$Variant;
    var $author$project$GenApp$element = F2(function(modName, app) {
      return A2($mdgriffith$elm_codegen$Elm$file, modName, _Utils_ap(_List_fromArray([
        A2($mdgriffith$elm_codegen$Elm$declaration, "main", $author$project$GenApp$appMain),
        A2($mdgriffith$elm_codegen$Elm$declaration, "init", A2($mdgriffith$elm_codegen$Elm$fn, A2($mdgriffith$elm_codegen$Elm$Arg$varWith, "flags", $author$project$Gen$Json$Decode$annotation_.hU), function(flags) {
          return A2($mdgriffith$elm_codegen$Elm$withType, $author$project$GenApp$updateTupleType, app.J(flags));
        })),
        A2($mdgriffith$elm_codegen$Elm$alias, "Model", app.av),
        A2($mdgriffith$elm_codegen$Elm$customType, "Msg", A2($elm$core$List$map, function(msg) {
          return A2($mdgriffith$elm_codegen$Elm$variantWith, msg.ag, msg.cB);
        }, app.fc)),
        A2($mdgriffith$elm_codegen$Elm$declaration, "update", A3($mdgriffith$elm_codegen$Elm$fn2, A2($mdgriffith$elm_codegen$Elm$Arg$varWith, "msg", $author$project$GenApp$msgType), A2($mdgriffith$elm_codegen$Elm$Arg$varWith, "model", $author$project$GenApp$modelType), F2(function(msg, model) {
          return A2($mdgriffith$elm_codegen$Elm$withType, $author$project$GenApp$updateTupleType, A3($mdgriffith$elm_codegen$Elm$Case$custom, msg, $author$project$GenApp$msgType, A2($elm$core$List$map, function(messageHandlers) {
            return messageHandlers.hP(model);
          }, app.fc)));
        }))),
        A2($mdgriffith$elm_codegen$Elm$declaration, "view", A2($mdgriffith$elm_codegen$Elm$fn, A2($mdgriffith$elm_codegen$Elm$Arg$varWith, "model", $author$project$GenApp$modelType), app.bu)),
        A2($mdgriffith$elm_codegen$Elm$declaration, "subscriptions", A2($mdgriffith$elm_codegen$Elm$fn, A2($mdgriffith$elm_codegen$Elm$Arg$varWith, "model", $author$project$GenApp$modelType), app.bk))
      ]), app.ix));
    });
    var $author$project$Interactive$inputToAnnotation = function(input) {
      switch (input.$) {
        case 0:
          return $mdgriffith$elm_codegen$Elm$Annotation$string;
        case 1:
          return $mdgriffith$elm_codegen$Elm$Annotation$bool;
        case 2:
          return $mdgriffith$elm_codegen$Elm$Annotation$int;
        case 3:
          return $mdgriffith$elm_codegen$Elm$Annotation$float;
        default:
          var inner = input.a;
          return $mdgriffith$elm_codegen$Elm$Annotation$maybe($author$project$Interactive$inputToAnnotation(inner));
      }
    };
    var $author$project$Interactive$fieldsToAnnotation = function(fields) {
      return $mdgriffith$elm_codegen$Elm$Annotation$record(A2($elm$core$List$map, function(_v0) {
        var name = _v0.a;
        var info = _v0.b;
        return _Utils_Tuple2(name, $author$project$Interactive$inputToAnnotation(info.E));
      }, fields));
    };
    var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccess = F2(function(a, b) {
      return { $: 20, a, b };
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$AttemptingGetOnTypeNameNotAnAlias = function(a) {
      return { $: 9, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$AttemptingToGetOnIncorrectType = function(a) {
      return { $: 8, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$getFieldFromList = F2(function(selector, fields) {
      getFieldFromList:
        while (true) {
          if (!fields.b) {
            return $elm$core$Maybe$Nothing;
          } else {
            var _v1 = fields.a;
            var _v2 = _v1.b;
            var _v3 = _v2.a;
            var fieldname = _v3.b;
            var _v4 = _v2.b;
            var contents = _v4.b;
            var remain = fields.b;
            if (_Utils_eq(fieldname, selector)) {
              return $elm$core$Maybe$Just(contents);
            } else {
              var $temp$selector = selector, $temp$fields = remain;
              selector = $temp$selector;
              fields = $temp$fields;
              continue getFieldFromList;
            }
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$inferRecordField = F2(function(index, _v0) {
      var nameOfRecord = _v0.fn;
      var fieldName = _v0.dz;
      var fieldType = $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType($mdgriffith$elm_codegen$Internal$Format$formatValue(_Utils_ap(fieldName, $mdgriffith$elm_codegen$Internal$Index$indexToString(index))));
      return $elm$core$Result$Ok({
        ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
        e: A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, nameOfRecord, A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord, $mdgriffith$elm_codegen$Internal$Compiler$nodify(nameOfRecord), $mdgriffith$elm_codegen$Internal$Compiler$nodify(_List_fromArray([
          $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2($mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName), $mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldType)))
        ]))), $elm$core$Dict$empty),
        hG: fieldType
      });
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$resolveField = F5(function(index, type_, aliases, inferences, fieldName) {
      resolveField:
        while (true) {
          if ($mdgriffith$elm_codegen$Internal$Index$typecheck(index)) {
            switch (type_.$) {
              case 4:
                var fields = type_.a;
                var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$getFieldFromList, fieldName, fields);
                if (!_v1.$) {
                  var ann = _v1.a;
                  return $elm$core$Result$Ok({ ib: aliases, e: inferences, hG: ann });
                } else {
                  return $elm$core$Result$Err(_List_fromArray([
                    $mdgriffith$elm_codegen$Internal$Compiler$CouldNotFindField({
                      iE: A2($elm$core$List$map, A2($elm$core$Basics$composeR, $mdgriffith$elm_codegen$Internal$Compiler$denode, A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $mdgriffith$elm_codegen$Internal$Compiler$denode)), fields),
                      ar: fieldName
                    })
                  ]));
                }
              case 5:
                var _v2 = type_.b;
                var fields = _v2.b;
                var _v3 = A2($mdgriffith$elm_codegen$Internal$Compiler$getFieldFromList, fieldName, fields);
                if (!_v3.$) {
                  var ann = _v3.a;
                  return $elm$core$Result$Ok({ ib: aliases, e: inferences, hG: ann });
                } else {
                  return $elm$core$Result$Err(_List_fromArray([
                    $mdgriffith$elm_codegen$Internal$Compiler$CouldNotFindField({
                      iE: A2($elm$core$List$map, A2($elm$core$Basics$composeR, $mdgriffith$elm_codegen$Internal$Compiler$denode, A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $mdgriffith$elm_codegen$Internal$Compiler$denode)), fields),
                      ar: fieldName
                    })
                  ]));
                }
              case 0:
                var nameOfRecord = type_.a;
                return A2($mdgriffith$elm_codegen$Internal$Compiler$inferRecordField, index, { dz: fieldName, fn: nameOfRecord });
              case 1:
                var nodedModAndName = type_.a;
                var _v4 = A2($mdgriffith$elm_codegen$Internal$Compiler$getAlias, nodedModAndName, aliases);
                if (_v4.$ === 1) {
                  return $elm$core$Result$Err(_List_fromArray([
                    $mdgriffith$elm_codegen$Internal$Compiler$AttemptingGetOnTypeNameNotAnAlias({ ar: fieldName, fy: type_ })
                  ]));
                } else {
                  var aliased = _v4.a;
                  var $temp$index = index, $temp$type_ = aliased.hf, $temp$aliases = aliases, $temp$inferences = inferences, $temp$fieldName = fieldName;
                  index = $temp$index;
                  type_ = $temp$type_;
                  aliases = $temp$aliases;
                  inferences = $temp$inferences;
                  fieldName = $temp$fieldName;
                  continue resolveField;
                }
              case 3:
                return $elm$core$Result$Err(_List_fromArray([
                  $mdgriffith$elm_codegen$Internal$Compiler$AttemptingToGetOnIncorrectType({ ar: fieldName, fy: type_ })
                ]));
              case 2:
                return $elm$core$Result$Err(_List_fromArray([
                  $mdgriffith$elm_codegen$Internal$Compiler$AttemptingToGetOnIncorrectType({ ar: fieldName, fy: type_ })
                ]));
              default:
                return $elm$core$Result$Err(_List_fromArray([
                  $mdgriffith$elm_codegen$Internal$Compiler$AttemptingToGetOnIncorrectType({ ar: fieldName, fy: type_ })
                ]));
            }
          } else {
            return $elm$core$Result$Err(_List_Nil);
          }
        }
    });
    var $mdgriffith$elm_codegen$Elm$get = F2(function(unformattedFieldName, recordExpression) {
      return function(index) {
        var fieldName = $mdgriffith$elm_codegen$Internal$Format$formatValue(unformattedFieldName);
        var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, recordExpression);
        var expr = _v0.b;
        return {
          bM: function() {
            var _v1 = expr.bM;
            if (!_v1.$) {
              var recordAnn = _v1.a;
              return A5($mdgriffith$elm_codegen$Internal$Compiler$resolveField, index, recordAnn.hG, recordAnn.ib, recordAnn.e, fieldName);
            } else {
              var otherwise = _v1;
              return otherwise;
            }
          }(),
          iH: A2($stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccess, $mdgriffith$elm_codegen$Internal$Compiler$nodify(expr.iH), $mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName)),
          c: expr.c
        };
      };
    });
    var $stil4m$elm_syntax$Elm$Syntax$Pattern$ListPattern = function(a) {
      return { $: 10, a };
    };
    var $mdgriffith$elm_codegen$Internal$Arg$item = F2(function(_v0, _v1) {
      var itemArg = _v0;
      var arg = _v1;
      return function(index) {
        var toSequence = arg(index);
        var itemDetails = itemArg(toSequence.at);
        var details = toSequence.cQ;
        var imports = details.c;
        var newAnnotation = A2($elm$core$Result$map, function(ann) {
          return { ib: ann.ib, e: ann.e, hG: ann.hG };
        }, details.bM);
        return {
          cQ: {
            bM: newAnnotation,
            c: imports,
            f$: function() {
              var _v2 = $mdgriffith$elm_codegen$Internal$Compiler$denode(details.f$);
              switch (_v2.$) {
                case 10:
                  var listItems = _v2.a;
                  return $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Pattern$ListPattern(_Utils_ap(listItems, _List_fromArray([itemDetails.cQ.f$]))));
                case 12:
                  var base = _v2.a;
                  var variantItems = _v2.b;
                  return $mdgriffith$elm_codegen$Internal$Compiler$nodify(A2($stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern, base, _Utils_ap(variantItems, _List_fromArray([itemDetails.cQ.f$]))));
                default:
                  return details.f$;
              }
            }()
          },
          at: $mdgriffith$elm_codegen$Internal$Index$next(itemDetails.at),
          hU: toSequence.hU(itemDetails.hU)
        };
      };
    });
    var $mdgriffith$elm_codegen$Elm$Arg$item = $mdgriffith$elm_codegen$Internal$Arg$item;
    var $author$project$Gen$Platform$Cmd$none = $mdgriffith$elm_codegen$Elm$value({
      bM: $elm$core$Maybe$Just(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_Nil, "Cmd", _List_fromArray([
        $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
      ]))),
      eo: _List_fromArray(["Cmd"]),
      ag: "none"
    });
    var $stil4m$elm_syntax$Elm$Syntax$Declaration$PortDeclaration = function(a) {
      return { $: 3, a };
    };
    var $mdgriffith$elm_codegen$Elm$groupAnn = function(ann) {
      return $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(_List_fromArray([ann])));
    };
    var $mdgriffith$elm_codegen$Elm$sub = A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2(_List_Nil, "Sub")), _List_fromArray([
      $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType("msg"))
    ]));
    var $mdgriffith$elm_codegen$Elm$portIncoming = F2(function(nameStr, args) {
      var name = $mdgriffith$elm_codegen$Internal$Format$formatDeclarationName(nameStr);
      return $mdgriffith$elm_codegen$Internal$Compiler$Declaration({
        aQ: $elm$core$Maybe$Nothing,
        du: $mdgriffith$elm_codegen$Internal$Compiler$NotExposed,
        c: A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, args),
        ag: name,
        _: function(_v0) {
          return {
            X: _List_Nil,
            iw: $stil4m$elm_syntax$Elm$Syntax$Declaration$PortDeclaration({
              ag: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name),
              aH: $mdgriffith$elm_codegen$Internal$Compiler$nodify(function() {
                if (!args.b) {
                  return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType("msg")), $mdgriffith$elm_codegen$Internal$Compiler$nodify($mdgriffith$elm_codegen$Elm$sub));
                } else {
                  return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, $mdgriffith$elm_codegen$Elm$groupAnn($mdgriffith$elm_codegen$Internal$Compiler$nodify($mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(A2($mdgriffith$elm_codegen$Elm$Annotation$function, args, $mdgriffith$elm_codegen$Elm$Annotation$var("msg"))))), $mdgriffith$elm_codegen$Internal$Compiler$nodify($mdgriffith$elm_codegen$Elm$sub));
                }
              }())
            }),
            jE: $elm$core$Maybe$Nothing
          };
        }
      });
    });
    var $mdgriffith$elm_codegen$Elm$cmd = A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2(_List_Nil, "Cmd")), _List_fromArray([
      $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType("msg"))
    ]));
    var $mdgriffith$elm_codegen$Elm$portOutgoing = F2(function(nameStr, ann) {
      var name = $mdgriffith$elm_codegen$Internal$Format$formatDeclarationName(nameStr);
      return $mdgriffith$elm_codegen$Internal$Compiler$Declaration({
        aQ: $elm$core$Maybe$Nothing,
        du: $mdgriffith$elm_codegen$Internal$Compiler$NotExposed,
        c: $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(ann),
        ag: name,
        _: function(_v0) {
          return {
            X: _List_Nil,
            iw: $stil4m$elm_syntax$Elm$Syntax$Declaration$PortDeclaration({
              ag: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name),
              aH: $mdgriffith$elm_codegen$Internal$Compiler$nodify(A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, $mdgriffith$elm_codegen$Internal$Compiler$nodify($mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(ann)), $mdgriffith$elm_codegen$Internal$Compiler$nodify($mdgriffith$elm_codegen$Elm$cmd)))
            }),
            jE: $elm$core$Maybe$Nothing
          };
        }
      });
    });
    var $mdgriffith$elm_codegen$Elm$Annotation$result = F2(function(err, ok) {
      return A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_Nil, "Result", _List_fromArray([err, ok]));
    });
    var $mdgriffith$elm_codegen$Internal$Arg$val = F2(function(index, name) {
      var typename = A2($mdgriffith$elm_codegen$Internal$Index$protectTypeName, name, index);
      var type_ = $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(typename);
      var annotation = $elm$core$Result$Ok({ ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: $elm$core$Dict$empty, hG: type_ });
      return _Utils_Tuple3(function(_v0) {
        return {
          bM: annotation,
          iH: A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, $mdgriffith$elm_codegen$Internal$Format$sanitize(name)),
          c: _List_Nil
        };
      }, annotation, type_);
    });
    var $mdgriffith$elm_codegen$Internal$Arg$var = function(rawName) {
      return function(index) {
        var imports = _List_Nil;
        var _v0 = A2($mdgriffith$elm_codegen$Internal$Index$getName, rawName, index);
        var name = _v0.a;
        var nameIndex = _v0.b;
        var _v1 = A2($mdgriffith$elm_codegen$Internal$Arg$val, nameIndex, name);
        var value = _v1.a;
        var annotation = _v1.b;
        return {
          cQ: {
            bM: annotation,
            c: imports,
            f$: $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(name))
          },
          at: $mdgriffith$elm_codegen$Internal$Index$next(nameIndex),
          hU: value
        };
      };
    };
    var $mdgriffith$elm_codegen$Elm$Arg$var = $mdgriffith$elm_codegen$Internal$Arg$var;
    var $mdgriffith$elm_codegen$Elm$Case$result = F2(function(mainExpression, branches) {
      return A3($mdgriffith$elm_codegen$Elm$Case$custom, mainExpression, A2($mdgriffith$elm_codegen$Elm$Annotation$result, $mdgriffith$elm_codegen$Elm$Annotation$var("err"), $mdgriffith$elm_codegen$Elm$Annotation$var("result")), _List_fromArray([
        A2($mdgriffith$elm_codegen$Elm$Case$branch, A2($mdgriffith$elm_codegen$Elm$Arg$item, $mdgriffith$elm_codegen$Elm$Arg$var(branches.i7.a), A2($mdgriffith$elm_codegen$Elm$Arg$customType, "Ok", $elm$core$Basics$identity)), function(val) {
          return A2($elm$core$Tuple$second, branches.i7, val);
        }),
        A2($mdgriffith$elm_codegen$Elm$Case$branch, A2($mdgriffith$elm_codegen$Elm$Arg$item, $mdgriffith$elm_codegen$Elm$Arg$var(branches.iD.a), A2($mdgriffith$elm_codegen$Elm$Arg$customType, "Err", $elm$core$Basics$identity)), function(val) {
          return A2($elm$core$Tuple$second, branches.iD, val);
        })
      ]));
    });
    var $author$project$Gen$Json$Encode$bool = function(boolArg_) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$bool]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
        eo: _List_fromArray(["Json", "Encode"]),
        ag: "bool"
      }), _List_fromArray([
        $mdgriffith$elm_codegen$Elm$bool(boolArg_)
      ]));
    };
    var $mdgriffith$elm_codegen$Elm$Annotation$list = function(inner) {
      return A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_Nil, "List", _List_fromArray([inner]));
    };
    var $author$project$Gen$Json$Encode$call_ = {
      bR: F2(function(arrayArg_, arrayArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil)),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Array"]), "Array", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
          eo: _List_fromArray(["Json", "Encode"]),
          ag: "array"
        }), _List_fromArray([arrayArg_, arrayArg_0]));
      }),
      ij: function(boolArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$bool]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
          eo: _List_fromArray(["Json", "Encode"]),
          ag: "bool"
        }), _List_fromArray([boolArg_]));
      },
      cS: F3(function(dictArg_, dictArg_0, dictArg_1) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("k")
            ]), $mdgriffith$elm_codegen$Elm$Annotation$string),
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("v")
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil)),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Dict"]), "Dict", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("k"),
              $mdgriffith$elm_codegen$Elm$Annotation$var("v")
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
          eo: _List_fromArray(["Json", "Encode"]),
          ag: "dict"
        }), _List_fromArray([dictArg_, dictArg_0, dictArg_1]));
      }),
      c8: F2(function(encodeArg_, encodeArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$int,
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil)
          ]), $mdgriffith$elm_codegen$Elm$Annotation$string)),
          eo: _List_fromArray(["Json", "Encode"]),
          ag: "encode"
        }), _List_fromArray([encodeArg_, encodeArg_0]));
      }),
      iI: function(floatArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$float]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
          eo: _List_fromArray(["Json", "Encode"]),
          ag: "float"
        }), _List_fromArray([floatArg_]));
      },
      iT: function(intArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$int]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
          eo: _List_fromArray(["Json", "Encode"]),
          ag: "int"
        }), _List_fromArray([intArg_]));
      },
      eO: F2(function(listArg_, listArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil)),
            $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$var("a"))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
          eo: _List_fromArray(["Json", "Encode"]),
          ag: "list"
        }), _List_fromArray([listArg_, listArg_0]));
      }),
      fw: function(objectArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil)))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
          eo: _List_fromArray(["Json", "Encode"]),
          ag: "object"
        }), _List_fromArray([objectArg_]));
      },
      gO: F2(function(setArg_, setArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil)),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Set"]), "Set", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
          eo: _List_fromArray(["Json", "Encode"]),
          ag: "set"
        }), _List_fromArray([setArg_, setArg_0]));
      }),
      jt: function(stringArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
          eo: _List_fromArray(["Json", "Encode"]),
          ag: "string"
        }), _List_fromArray([stringArg_]));
      }
    };
    var $author$project$Interactive$encodeDefault = F2(function(input, expr) {
      encodeDefault:
        while (true) {
          switch (input.$) {
            case 0:
              return $author$project$Gen$Json$Encode$call_.jt(expr);
            case 1:
              return $author$project$Gen$Json$Encode$call_.ij(expr);
            case 2:
              return $author$project$Gen$Json$Encode$call_.iT(expr);
            case 3:
              return $author$project$Gen$Json$Encode$call_.iI(expr);
            default:
              var inner = input.a;
              var $temp$input = inner, $temp$expr = expr;
              input = $temp$input;
              expr = $temp$expr;
              continue encodeDefault;
          }
        }
    });
    var $author$project$Interactive$inputToString = function(input) {
      inputToString:
        while (true) {
          switch (input.$) {
            case 0:
              return "string";
            case 1:
              return "bool";
            case 2:
              return "int";
            case 3:
              return "float";
            default:
              var inner = input.a;
              var $temp$input = inner;
              input = $temp$input;
              continue inputToString;
          }
        }
    };
    var $author$project$Interactive$isRequired = function(input) {
      if (input.$ === 4) {
        return false;
      } else {
        return true;
      }
    };
    var $stil4m$elm_syntax$Elm$Syntax$Expression$Literal = function(a) {
      return { $: 11, a };
    };
    var $mdgriffith$elm_codegen$Internal$Types$string = A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Types$nodify(_Utils_Tuple2(_List_Nil, "String")), _List_Nil);
    var $mdgriffith$elm_codegen$Elm$string = function(literal) {
      return function(_v0) {
        return {
          bM: $elm$core$Result$Ok({ ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: $elm$core$Dict$empty, hG: $mdgriffith$elm_codegen$Internal$Types$string }),
          iH: $stil4m$elm_syntax$Elm$Syntax$Expression$Literal(literal),
          c: _List_Nil
        };
      };
    };
    var $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression = function(a) {
      return { $: 13, a };
    };
    var $mdgriffith$elm_codegen$Elm$tuple = F2(function(oneExp, twoExp) {
      return function(index) {
        var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, oneExp);
        var oneIndex = _v0.a;
        var one = _v0.b;
        var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, oneIndex, twoExp);
        var two = _v1.b;
        return {
          bM: A3($elm$core$Result$map2, F2(function(oneA, twoA) {
            return {
              ib: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, twoA.ib, oneA.ib),
              e: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, twoA.e, oneA.e),
              hG: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(_List_fromArray([
                $mdgriffith$elm_codegen$Internal$Compiler$nodify(oneA.hG),
                $mdgriffith$elm_codegen$Internal$Compiler$nodify(twoA.hG)
              ]))
            };
          }), one.bM, two.bM),
          iH: $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression(_List_fromArray([
            $mdgriffith$elm_codegen$Internal$Compiler$nodify(one.iH),
            $mdgriffith$elm_codegen$Internal$Compiler$nodify(two.iH)
          ])),
          c: _Utils_ap(one.c, two.c)
        };
      };
    });
    var $author$project$Interactive$keyValue = F2(function(key, value) {
      return A2($mdgriffith$elm_codegen$Elm$tuple, $mdgriffith$elm_codegen$Elm$string(key), value);
    });
    var $stil4m$elm_syntax$Elm$Syntax$Expression$ListExpr = function(a) {
      return { $: 19, a };
    };
    var $mdgriffith$elm_codegen$Internal$Compiler$MismatchedList = F2(function(a, b) {
      return { $: 0, a, b };
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$unifyHelper = F2(function(exps, existing) {
      unifyHelper:
        while (true) {
          if (!exps.b) {
            return $elm$core$Result$Ok(existing);
          } else {
            var top = exps.a;
            var remain = exps.b;
            var _v1 = top.bM;
            if (!_v1.$) {
              var ann = _v1.a;
              var _v2 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, ann.ib, ann.e, ann.hG, existing.hG);
              if (_v2.b.$ === 1) {
                return $elm$core$Result$Err(_List_fromArray([
                  A2($mdgriffith$elm_codegen$Internal$Compiler$MismatchedList, ann.hG, existing.hG)
                ]));
              } else {
                var cache = _v2.a;
                var _new = _v2.b.a;
                var $temp$exps = remain, $temp$existing = {
                  ib: existing.ib,
                  e: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, existing.e, cache),
                  hG: _new
                };
                exps = $temp$exps;
                existing = $temp$existing;
                continue unifyHelper;
              }
            } else {
              var err = _v1.a;
              return $elm$core$Result$Err(err);
            }
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$unify = function(exps) {
      if (!exps.b) {
        return $elm$core$Result$Ok({
          ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
          e: $elm$core$Dict$empty,
          hG: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType("a")
        });
      } else {
        var top = exps.a;
        var remain = exps.b;
        var _v1 = top.bM;
        if (!_v1.$) {
          var ann = _v1.a;
          return A2($mdgriffith$elm_codegen$Internal$Compiler$unifyHelper, remain, ann);
        } else {
          var err = _v1.a;
          return $elm$core$Result$Err(err);
        }
      }
    };
    var $mdgriffith$elm_codegen$Elm$list = function(exprs) {
      return $mdgriffith$elm_codegen$Internal$Compiler$expression(function(index) {
        var exprDetails = A2($mdgriffith$elm_codegen$Internal$Compiler$thread, index, exprs);
        return {
          bM: A2($elm$core$Result$map, function(inner) {
            return {
              ib: inner.ib,
              e: inner.e,
              hG: A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2(_List_Nil, "List")), _List_fromArray([
                $mdgriffith$elm_codegen$Internal$Compiler$nodify(inner.hG)
              ]))
            };
          }, $mdgriffith$elm_codegen$Internal$Compiler$unify(exprDetails)),
          iH: $stil4m$elm_syntax$Elm$Syntax$Expression$ListExpr(A2($elm$core$List$map, A2($elm$core$Basics$composeR, function($) {
            return $.iH;
          }, $mdgriffith$elm_codegen$Internal$Compiler$nodify), exprDetails)),
          c: A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getImports, exprDetails)
        };
      });
    };
    var $author$project$Gen$Json$Encode$object = function(objectArg_) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
          $mdgriffith$elm_codegen$Elm$Annotation$list(A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil)))
        ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
        eo: _List_fromArray(["Json", "Encode"]),
        ag: "object"
      }), _List_fromArray([
        $mdgriffith$elm_codegen$Elm$list(objectArg_)
      ]));
    };
    var $author$project$Gen$Json$Encode$string = function(stringArg_) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Json", "Encode"]), "Value", _List_Nil))),
        eo: _List_fromArray(["Json", "Encode"]),
        ag: "string"
      }), _List_fromArray([
        $mdgriffith$elm_codegen$Elm$string(stringArg_)
      ]));
    };
    var $author$project$Interactive$encodeControls = function(fields) {
      return $author$project$Gen$Json$Encode$object(A2($elm$core$List$map, function(_v0) {
        var name = _v0.a;
        var info = _v0.b;
        return A2($author$project$Interactive$keyValue, name, $author$project$Gen$Json$Encode$object(_List_fromArray([
          A2($author$project$Interactive$keyValue, "name", $author$project$Gen$Json$Encode$string(name)),
          A2($author$project$Interactive$keyValue, "type", $author$project$Gen$Json$Encode$string($author$project$Interactive$inputToString(info.E))),
          A2($author$project$Interactive$keyValue, "required", $author$project$Gen$Json$Encode$bool($author$project$Interactive$isRequired(info.E))),
          A2($author$project$Interactive$keyValue, "default", A2($author$project$Interactive$encodeDefault, info.E, info.J))
        ])));
      }, fields));
    };
    var $author$project$Interactive$sendControlsUpdated = F2(function(portName, fields) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({ bM: $elm$core$Maybe$Nothing, eo: _List_Nil, ag: portName }), _List_fromArray([
        $author$project$Interactive$encodeControls(fields)
      ]));
    });
    var $stil4m$elm_syntax$Elm$Syntax$Pattern$AllPattern = { $: 0 };
    var $stil4m$elm_syntax$Elm$Syntax$Expression$UnitExpr = { $: 0 };
    var $mdgriffith$elm_codegen$Internal$Compiler$inference = function(type_) {
      return { ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: $elm$core$Dict$empty, hG: type_ };
    };
    var $mdgriffith$elm_codegen$Internal$Arg$ignore = function(index) {
      var imports = _List_Nil;
      var annotation = $elm$core$Result$Ok($mdgriffith$elm_codegen$Internal$Compiler$inference($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit));
      return {
        cQ: {
          bM: annotation,
          c: imports,
          f$: $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Pattern$AllPattern)
        },
        at: $mdgriffith$elm_codegen$Internal$Index$next(index),
        hU: function(_v0) {
          return {
            bM: $elm$core$Result$Ok($mdgriffith$elm_codegen$Internal$Compiler$inference($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit)),
            iH: $stil4m$elm_syntax$Elm$Syntax$Expression$UnitExpr,
            c: imports
          };
        }
      };
    };
    var $mdgriffith$elm_codegen$Elm$Arg$ignore = $mdgriffith$elm_codegen$Internal$Arg$ignore;
    var $stil4m$elm_syntax$Elm$Syntax$Pattern$StringPattern = function(a) {
      return { $: 3, a };
    };
    var $mdgriffith$elm_codegen$Internal$Arg$string = function(str) {
      return function(index) {
        var imports = _List_Nil;
        var annotation = $elm$core$Result$Ok({ ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: $elm$core$Dict$empty, hG: $mdgriffith$elm_codegen$Internal$Types$string });
        return {
          cQ: {
            bM: annotation,
            c: imports,
            f$: $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Pattern$StringPattern(str))
          },
          at: $mdgriffith$elm_codegen$Internal$Index$next(index),
          hU: function(_v0) {
            return {
              bM: annotation,
              iH: $stil4m$elm_syntax$Elm$Syntax$Expression$Literal(str),
              c: imports
            };
          }
        };
      };
    };
    var $mdgriffith$elm_codegen$Elm$Arg$string = $mdgriffith$elm_codegen$Internal$Arg$string;
    var $mdgriffith$elm_codegen$Elm$Case$string = F2(function(mainExpression, config) {
      return A3($mdgriffith$elm_codegen$Elm$Case$custom, mainExpression, $mdgriffith$elm_codegen$Elm$Annotation$string, _Utils_ap(A2($elm$core$List$map, function(_v0) {
        var key = _v0.a;
        var value = _v0.b;
        return A2($mdgriffith$elm_codegen$Elm$Case$branch, $mdgriffith$elm_codegen$Elm$Arg$string(key), function(_v1) {
          return value;
        });
      }, config.il), _List_fromArray([
        A2($mdgriffith$elm_codegen$Elm$Case$branch, $mdgriffith$elm_codegen$Elm$Arg$ignore, function(_v2) {
          return config.jc;
        })
      ])));
    });
    var $stil4m$elm_syntax$Elm$Syntax$Expression$LetDestructuring = F2(function(a, b) {
      return { $: 1, a, b };
    });
    var $stil4m$elm_syntax$Elm$Syntax$Expression$LetExpression = function(a) {
      return { $: 15, a };
    };
    var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordUpdateExpression = F2(function(a, b) {
      return { $: 22, a, b };
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$RecordUpdateIncorrectFields = function(a) {
      return { $: 11, a };
    };
    var $mdgriffith$elm_codegen$Elm$presentAndMatching = F3(function(fieldName, _v0, existingFields) {
      return A2($elm$core$List$any, function(_v1) {
        var _v2 = _v1.b;
        var _v3 = _v2.a;
        var existingFieldName = _v3.b;
        return _Utils_eq(fieldName, existingFieldName);
      }, existingFields);
    });
    var $mdgriffith$elm_codegen$Elm$verifyFieldsHelper = F2(function(existingFields, updatedFields) {
      if (!updatedFields.b) {
        return true;
      } else {
        var _v1 = updatedFields.a;
        var fieldName = _v1.a;
        var fieldInference = _v1.b;
        var remain = updatedFields.b;
        return A3($mdgriffith$elm_codegen$Elm$presentAndMatching, fieldName, fieldInference, existingFields) && A2($mdgriffith$elm_codegen$Elm$verifyFieldsHelper, existingFields, remain);
      }
    });
    var $mdgriffith$elm_codegen$Elm$verifyFields = F2(function(updatedFields, existingFields) {
      return A2($mdgriffith$elm_codegen$Elm$verifyFieldsHelper, existingFields, updatedFields) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just($mdgriffith$elm_codegen$Internal$Compiler$RecordUpdateIncorrectFields({
        ie: A2($elm$core$List$map, function(_v0) {
          var fieldName = _v0.a;
          var fieldInference = _v0.b;
          return _Utils_Tuple2(fieldName, fieldInference.hG);
        }, updatedFields),
        iE: A2($elm$core$List$map, function(_v1) {
          var _v2 = _v1.b;
          var _v3 = _v2.a;
          var fieldName = _v3.b;
          var _v4 = _v2.b;
          var fieldInference = _v4.b;
          return _Utils_Tuple2(fieldName, fieldInference);
        }, existingFields)
      }));
    });
    var $mdgriffith$elm_codegen$Elm$updateRecord = F2(function(fields, recordExpression) {
      return $mdgriffith$elm_codegen$Internal$Compiler$expression(function(index) {
        var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, recordExpression);
        var recordIndex = _v0.a;
        var recordExp = _v0.b;
        var _v1 = A3($elm$core$List$foldl, F2(function(_v2, _v3) {
          var fieldNameUnformatted = _v2.a;
          var fieldExp = _v2.b;
          var currentIndex = _v3.a;
          var fieldAnnotationResult = _v3.b;
          var items = _v3.c;
          var fieldName = $mdgriffith$elm_codegen$Internal$Format$formatValue(fieldNameUnformatted);
          var _v4 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, currentIndex, fieldExp);
          var newIndex = _v4.a;
          var exp = _v4.b;
          var currentFieldAnnotations = function() {
            if (!fieldAnnotationResult.$) {
              var fieldAnns = fieldAnnotationResult.a;
              var _v6 = exp.bM;
              if (!_v6.$) {
                var fs = _v6.a;
                return $elm$core$Result$Ok(A2($elm$core$List$cons, _Utils_Tuple2(fieldName, fs), fieldAnns));
              } else {
                var newErr = _v6.a;
                return $elm$core$Result$Err(newErr);
              }
            } else {
              var err = fieldAnnotationResult.a;
              var _v7 = exp.bM;
              if (!_v7.$) {
                return fieldAnnotationResult;
              } else {
                var newErr = _v7.a;
                return $elm$core$Result$Err(_Utils_ap(err, newErr));
              }
            }
          }();
          return _Utils_Tuple3(newIndex, currentFieldAnnotations, A2($elm$core$List$cons, _Utils_Tuple2(fieldName, exp), items));
        }), _Utils_Tuple3(recordIndex, $elm$core$Result$Ok(_List_Nil), _List_Nil), fields);
        var fieldIndex = _v1.a;
        var fieldAnnotationsGathered = _v1.b;
        var fieldDetails = _v1.c;
        return {
          bM: function() {
            if (fieldAnnotationsGathered.$ === 1) {
              var fieldErrors = fieldAnnotationsGathered.a;
              return $elm$core$Result$Err(fieldErrors);
            } else {
              var verifiedFieldAnnotations = fieldAnnotationsGathered.a;
              var _v9 = recordExp.bM;
              if (!_v9.$) {
                var recordAnn = _v9.a;
                var _v10 = recordAnn.hG;
                switch (_v10.$) {
                  case 4:
                    var existingFields = _v10.a;
                    var _v11 = A2($mdgriffith$elm_codegen$Elm$verifyFields, verifiedFieldAnnotations, existingFields);
                    if (_v11.$ === 1) {
                      return recordExp.bM;
                    } else {
                      var err = _v11.a;
                      return $elm$core$Result$Err(_List_fromArray([err]));
                    }
                  case 0:
                    var nameOfRecord = _v10.a;
                    return $elm$core$Result$Ok({
                      ib: recordAnn.ib,
                      e: A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, nameOfRecord, A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord, $mdgriffith$elm_codegen$Internal$Compiler$nodify(nameOfRecord), $mdgriffith$elm_codegen$Internal$Compiler$nodify(A2($elm$core$List$map, function(_v12) {
                        var fieldName = _v12.a;
                        var inference = _v12.b;
                        return $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2($mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName), $mdgriffith$elm_codegen$Internal$Compiler$nodify(inference.hG)));
                      }, verifiedFieldAnnotations))), recordAnn.e),
                      hG: recordAnn.hG
                    });
                  default:
                    return recordExp.bM;
                }
              } else {
                var otherwise = _v9;
                return otherwise;
              }
            }
          }(),
          iH: function() {
            var _v13 = recordExp.iH;
            if (_v13.$ === 3) {
              var name = _v13.b;
              return A2($stil4m$elm_syntax$Elm$Syntax$Expression$RecordUpdateExpression, $mdgriffith$elm_codegen$Internal$Compiler$nodify(name), A2($elm$core$List$map, function(_v14) {
                var fieldName = _v14.a;
                var expDetails = _v14.b;
                return $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2($mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName), $mdgriffith$elm_codegen$Internal$Compiler$nodify(expDetails.iH)));
              }, $elm$core$List$reverse(fieldDetails)));
            } else {
              var name = "record" + $mdgriffith$elm_codegen$Internal$Index$indexToString(fieldIndex);
              return $stil4m$elm_syntax$Elm$Syntax$Expression$LetExpression({
                ix: _List_fromArray([
                  $mdgriffith$elm_codegen$Internal$Compiler$nodify(A2($stil4m$elm_syntax$Elm$Syntax$Expression$LetDestructuring, $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(name)), $mdgriffith$elm_codegen$Internal$Compiler$nodify(recordExp.iH)))
                ]),
                iH: $mdgriffith$elm_codegen$Internal$Compiler$nodify(A2($stil4m$elm_syntax$Elm$Syntax$Expression$RecordUpdateExpression, $mdgriffith$elm_codegen$Internal$Compiler$nodify(name), A2($elm$core$List$map, function(_v15) {
                  var fieldName = _v15.a;
                  var expDetails = _v15.b;
                  return $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2($mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName), $mdgriffith$elm_codegen$Internal$Compiler$nodify(expDetails.iH)));
                }, fieldDetails)))
              });
            }
          }(),
          c: A2($elm$core$List$concatMap, A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $mdgriffith$elm_codegen$Internal$Compiler$getImports), fieldDetails)
        };
      });
    });
    var $author$project$Interactive$generateSingle = F2(function(moduleName, interact) {
      var moduleId = A2($elm$core$String$join, "", moduleName);
      var portInName = "propertyUpdated" + moduleId;
      var portOutName = "controlsUpdated" + moduleId;
      return A2($author$project$GenApp$element, moduleName, {
        ix: _List_fromArray([
          A2($mdgriffith$elm_codegen$Elm$portIncoming, portInName, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("name", $mdgriffith$elm_codegen$Elm$Annotation$string),
              _Utils_Tuple2("value", $author$project$Gen$Json$Encode$annotation_.hU)
            ]))
          ])),
          A2($mdgriffith$elm_codegen$Elm$portOutgoing, portOutName, $author$project$Gen$Json$Encode$annotation_.hU)
        ]),
        J: function(flags) {
          return A2($mdgriffith$elm_codegen$Elm$tuple, $mdgriffith$elm_codegen$Elm$record(A2($elm$core$List$map, function(_v0) {
            var name = _v0.a;
            var info = _v0.b;
            return _Utils_Tuple2(name, info.J);
          }, interact.aU)), A2($author$project$Interactive$sendControlsUpdated, portOutName, interact.aU));
        },
        fc: _Utils_ap(A2($elm$core$List$map, function(_v1) {
          var fieldName = _v1.a;
          var info = _v1.b;
          var variantName = "Property" + ($author$project$Interactive$capitalize(fieldName) + "Updated");
          return {
            cB: _List_fromArray([
              $author$project$Interactive$inputToAnnotation(info.E)
            ]),
            ag: variantName,
            hP: function(model) {
              return A2($mdgriffith$elm_codegen$Elm$Case$branch, A2($mdgriffith$elm_codegen$Elm$Arg$item, A2($mdgriffith$elm_codegen$Elm$Arg$varWith, "value", $author$project$Interactive$inputToAnnotation(info.E)), A2($mdgriffith$elm_codegen$Elm$Arg$customType, variantName, $elm$core$Basics$identity)), function(value) {
                return A2($mdgriffith$elm_codegen$Elm$tuple, A2($mdgriffith$elm_codegen$Elm$updateRecord, _List_fromArray([
                  _Utils_Tuple2(fieldName, value)
                ]), model), $author$project$Gen$Platform$Cmd$none);
              });
            }
          };
        }, interact.aU), _List_fromArray([
          {
            cB: _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]),
            ag: "UnknownProperty",
            hP: function(model) {
              return A2($mdgriffith$elm_codegen$Elm$Case$branch, A2($mdgriffith$elm_codegen$Elm$Arg$item, A2($mdgriffith$elm_codegen$Elm$Arg$varWith, "name", $mdgriffith$elm_codegen$Elm$Annotation$string), A2($mdgriffith$elm_codegen$Elm$Arg$customType, "UnknownProperty", $elm$core$Basics$identity)), function(_v2) {
                return A2($mdgriffith$elm_codegen$Elm$tuple, model, $author$project$Gen$Platform$Cmd$none);
              });
            }
          }
        ])),
        av: $author$project$Interactive$fieldsToAnnotation(interact.aU),
        bk: function(_v3) {
          return A2($mdgriffith$elm_codegen$Elm$withType, A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_Nil, "Sub", _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, "Msg")
          ])), A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({ bM: $elm$core$Maybe$Nothing, eo: _List_Nil, ag: portInName }), _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$fn, A2($mdgriffith$elm_codegen$Elm$Arg$varWith, "payload", $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("name", $mdgriffith$elm_codegen$Elm$Annotation$string),
              _Utils_Tuple2("value", $author$project$Gen$Json$Encode$annotation_.hU)
            ]))), function(payload) {
              return A2($mdgriffith$elm_codegen$Elm$Case$string, A2($mdgriffith$elm_codegen$Elm$get, "name", payload), {
                il: A2($elm$core$List$map, function(_v4) {
                  var fieldName = _v4.a;
                  var info = _v4.b;
                  var variantName = "Property" + ($author$project$Interactive$capitalize(fieldName) + "Updated");
                  return _Utils_Tuple2(fieldName, A2($mdgriffith$elm_codegen$Elm$Case$result, A2($author$project$Interactive$decode, info.E, A2($mdgriffith$elm_codegen$Elm$get, "value", payload)), {
                    iD: _Utils_Tuple2("err", function(_v5) {
                      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$val("UnknownProperty"), _List_fromArray([
                        A2($mdgriffith$elm_codegen$Elm$get, "name", payload)
                      ]));
                    }),
                    i7: _Utils_Tuple2("val", function(decodedValue) {
                      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$val(variantName), _List_fromArray([decodedValue]));
                    })
                  }));
                }, interact.aU),
                jc: A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$val("UnknownProperty"), _List_fromArray([
                  A2($mdgriffith$elm_codegen$Elm$get, "name", payload)
                ]))
              });
            })
          ])));
        },
        bu: function(model) {
          return A2($mdgriffith$elm_codegen$Elm$withType, $author$project$Ui$annotation_.c5(A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, "Msg")), interact.bu({
            io: $mdgriffith$elm_codegen$Elm$bool(true),
            av: model,
            a6: $mdgriffith$elm_codegen$Elm$value({
              bM: $elm$core$Maybe$Nothing,
              eo: _List_Nil,
              ag: $author$project$Interactive$capitalize(interact.ag)
            })
          }));
        }
      });
    });
    var $author$project$Interactive$InputBool = { $: 1 };
    var $author$project$Interactive$bool = $author$project$Interactive$InputBool;
    var $author$project$Example$Interactive$Build$genIdentity = A2($mdgriffith$elm_codegen$Elm$fn, $mdgriffith$elm_codegen$Elm$Arg$var("a"), function(a) {
      return a;
    });
    var $stil4m$elm_syntax$Elm$Syntax$Expression$IfBlock = F3(function(a, b, c) {
      return { $: 4, a, b, c };
    });
    var $mdgriffith$elm_codegen$Elm$ifThen = F3(function(condition, thenBranch, elseBranch) {
      return function(index) {
        var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, condition);
        var condIndex = _v0.a;
        var cond = _v0.b;
        var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, condIndex, thenBranch);
        var thenIndex = _v1.a;
        var thenB = _v1.b;
        var _v2 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, thenIndex, elseBranch);
        var elseB = _v2.b;
        return {
          bM: thenB.bM,
          iH: A3($stil4m$elm_syntax$Elm$Syntax$Expression$IfBlock, $mdgriffith$elm_codegen$Internal$Compiler$nodify(cond.iH), $mdgriffith$elm_codegen$Internal$Compiler$nodify(thenB.iH), $mdgriffith$elm_codegen$Internal$Compiler$nodify(elseB.iH)),
          c: _Utils_ap(cond.c, _Utils_ap(thenB.c, elseB.c))
        };
      };
    });
    var $mdgriffith$elm_codegen$Elm$Op$BinOp = F3(function(a, b, c) {
      return { $: 0, a, b, c };
    });
    var $stil4m$elm_syntax$Elm$Syntax$Infix$Left = 0;
    var $stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication = F4(function(a, b, c, d) {
      return { $: 2, a, b, c, d };
    });
    var $mdgriffith$elm_codegen$Elm$Op$applyPipe = F4(function(_v0, infixAnnotation, l, r) {
      var symbol = _v0.a;
      var dir = _v0.b;
      return function(index) {
        var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, l);
        var leftIndex = _v1.a;
        var left = _v1.b;
        var _v2 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, leftIndex, r);
        var right = _v2.b;
        return {
          bM: A3($mdgriffith$elm_codegen$Internal$Compiler$applyType, index, $elm$core$Result$Ok({ ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: $elm$core$Dict$empty, hG: infixAnnotation }), _List_fromArray([left, right])),
          iH: A4($stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication, symbol, dir, $mdgriffith$elm_codegen$Internal$Compiler$nodify(left.iH), $mdgriffith$elm_codegen$Internal$Compiler$nodify(right.iH)),
          c: _Utils_ap(left.c, right.c)
        };
      };
    });
    var $mdgriffith$elm_codegen$Internal$Types$function = F2(function(args, _return) {
      return A3($elm$core$List$foldr, F2(function(ann, fn) {
        return A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation, $mdgriffith$elm_codegen$Internal$Types$nodify(ann), $mdgriffith$elm_codegen$Internal$Types$nodify(fn));
      }), _return, args);
    });
    var $mdgriffith$elm_codegen$Internal$Types$formatValue = function(str) {
      var formatted = _Utils_eq($elm$core$String$toUpper(str), str) ? $elm$core$String$toLower(str) : _Utils_ap($elm$core$String$toLower(A2($elm$core$String$left, 1, str)), A2($elm$core$String$dropLeft, 1, str));
      return $mdgriffith$elm_codegen$Internal$Format$sanitize(formatted);
    };
    var $mdgriffith$elm_codegen$Internal$Types$var = function(name) {
      return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType($mdgriffith$elm_codegen$Internal$Types$formatValue(name));
    };
    var $mdgriffith$elm_codegen$Elm$Op$pipe = F2(function(r, l) {
      return A4($mdgriffith$elm_codegen$Elm$Op$applyPipe, A3($mdgriffith$elm_codegen$Elm$Op$BinOp, "|>", 0, 0), A2($mdgriffith$elm_codegen$Internal$Types$function, _List_fromArray([
        $mdgriffith$elm_codegen$Internal$Types$var("a"),
        A2($mdgriffith$elm_codegen$Internal$Types$function, _List_fromArray([
          $mdgriffith$elm_codegen$Internal$Types$var("a")
        ]), $mdgriffith$elm_codegen$Internal$Types$var("b"))
      ]), $mdgriffith$elm_codegen$Internal$Types$var("b")), l, r);
    });
    var $author$project$Example$Interactive$Build$applyBuilder = F2(function(_v0, value) {
      var includeBuilder = _v0.a;
      var builder = _v0.b;
      return A2($mdgriffith$elm_codegen$Elm$Op$pipe, A3($mdgriffith$elm_codegen$Elm$ifThen, includeBuilder, builder, $author$project$Example$Interactive$Build$genIdentity), value);
    });
    var $stil4m$elm_syntax$Elm$Syntax$Expression$Floatable = function(a) {
      return { $: 9, a };
    };
    var $mdgriffith$elm_codegen$Internal$Types$float = A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Types$nodify(_Utils_Tuple2(_List_Nil, "Float")), _List_Nil);
    var $mdgriffith$elm_codegen$Elm$float = function(floatVal) {
      return function(_v0) {
        return {
          bM: $elm$core$Result$Ok({ ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: $elm$core$Dict$empty, hG: $mdgriffith$elm_codegen$Internal$Types$float }),
          iH: $stil4m$elm_syntax$Elm$Syntax$Expression$Floatable(floatVal),
          c: _List_Nil
        };
      };
    };
    var $author$project$Interactive$InputFloat = { $: 3 };
    var $author$project$Interactive$float = $author$project$Interactive$InputFloat;
    var $author$project$Example$Type$matchesName = F2(function(name, tipe) {
      switch (tipe.$) {
        case 3:
          var typeName = tipe.a;
          return _Utils_eq(typeName, name);
        case 1:
          var arg = tipe.a;
          var result = tipe.b;
          return A2($author$project$Example$Type$matchesName, name, arg) || A2($author$project$Example$Type$matchesName, name, result);
        default:
          return false;
      }
    });
    var $author$project$Example$Type$isBuilderOfName = F2(function(name, tipe) {
      isBuilderOfName:
        while (true) {
          if (tipe.$ === 1) {
            var arg = tipe.a;
            var result = tipe.b;
            if (A2($author$project$Example$Type$matchesName, name, arg) && A2($author$project$Example$Type$matchesName, name, result)) {
              return true;
            } else {
              var $temp$name = name, $temp$tipe = result;
              name = $temp$name;
              tipe = $temp$tipe;
              continue isBuilderOfName;
            }
          } else {
            return false;
          }
        }
    });
    var $author$project$Example$Type$getBuilderOf = F2(function(name, doc) {
      return A2($author$project$Example$Type$isBuilderOfName, name, doc.ak) ? $elm$core$Maybe$Just(doc) : $elm$core$Maybe$Nothing;
    });
    var $author$project$Interactive$Field = F2(function(a, b) {
      return { $: 0, a, b };
    });
    var $author$project$Interactive$field = $author$project$Interactive$Field;
    var $author$project$Example$Interactive$Build$getVal = F3(function(nameBase, options, context) {
      var arg = nameBase;
      return {
        iu: _Utils_update(context, {
          P: context.P + 1,
          js: A2($elm$core$List$cons, A2($author$project$Interactive$field, arg, options), context.js)
        }),
        c0: A2($mdgriffith$elm_codegen$Elm$get, arg, context.av)
      };
    });
    var $author$project$Example$Interactive$Build$getValProtected = F3(function(nameBase, options, context) {
      var arg = _Utils_ap(nameBase, $elm$core$String$fromInt(context.P));
      return {
        iu: _Utils_update(context, {
          P: context.P + 1,
          js: A2($elm$core$List$cons, A2($author$project$Interactive$field, arg, options), context.js)
        }),
        c0: A2($mdgriffith$elm_codegen$Elm$get, arg, context.av)
      };
    });
    var $stil4m$elm_syntax$Elm$Syntax$Expression$Integer = function(a) {
      return { $: 7, a };
    };
    var $mdgriffith$elm_codegen$Internal$Types$int = A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Types$nodify(_Utils_Tuple2(_List_Nil, "Int")), _List_Nil);
    var $mdgriffith$elm_codegen$Elm$int = function(intVal) {
      return function(_v0) {
        return {
          bM: $elm$core$Result$Ok({ ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, e: $elm$core$Dict$empty, hG: $mdgriffith$elm_codegen$Internal$Types$int }),
          iH: $stil4m$elm_syntax$Elm$Syntax$Expression$Integer(intVal),
          c: _List_Nil
        };
      };
    };
    var $author$project$Interactive$InputInt = { $: 2 };
    var $author$project$Interactive$int = $author$project$Interactive$InputInt;
    var $author$project$Example$Type$isCreatorOf = F2(function(name, tipe) {
      isCreatorOf:
        while (true) {
          switch (tipe.$) {
            case 3:
              var typeName = tipe.a;
              return _Utils_eq(typeName, name);
            case 1:
              var arg = tipe.a;
              var result = tipe.b;
              if (A2($author$project$Example$Type$matchesName, name, arg)) {
                return false;
              } else {
                var $temp$name = name, $temp$tipe = result;
                name = $temp$name;
                tipe = $temp$tipe;
                continue isCreatorOf;
              }
            default:
              return false;
          }
        }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$getAnnotation = function(exp) {
      return exp.bM;
    };
    var $mdgriffith$elm_codegen$Elm$maybe = function(maybeContent) {
      return function(index) {
        if (maybeContent.$ === 1) {
          return {
            bM: $elm$core$Result$Ok(A2($mdgriffith$elm_codegen$Internal$Compiler$getInnerInference, index, $mdgriffith$elm_codegen$Elm$Annotation$maybe($mdgriffith$elm_codegen$Elm$Annotation$var("a")))),
            iH: A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, "Nothing"),
            c: _List_Nil
          };
        } else {
          var contentExp = maybeContent.a;
          var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, contentExp);
          var content = _v1.b;
          return {
            bM: A2($elm$core$Result$map, function(ann) {
              return {
                ib: ann.ib,
                e: ann.e,
                hG: A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, $mdgriffith$elm_codegen$Internal$Compiler$nodify(_Utils_Tuple2(_List_Nil, "Maybe")), _List_fromArray([
                  $mdgriffith$elm_codegen$Internal$Compiler$nodify(ann.hG)
                ]))
              };
            }, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotation(content)),
            iH: $stil4m$elm_syntax$Elm$Syntax$Expression$Application(_List_fromArray([
              $mdgriffith$elm_codegen$Internal$Compiler$nodify(A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, "Just")),
              $mdgriffith$elm_codegen$Internal$Compiler$nodify($stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression($mdgriffith$elm_codegen$Internal$Compiler$nodify(content.iH)))
            ])),
            c: $mdgriffith$elm_codegen$Internal$Compiler$getImports(content)
          };
        }
      };
    };
    var $mdgriffith$elm_codegen$Elm$just = function(content) {
      return $mdgriffith$elm_codegen$Elm$maybe($elm$core$Maybe$Just(content));
    };
    var $author$project$Interactive$log = $mdgriffith$elm_codegen$Elm$value({ bM: $elm$core$Maybe$Nothing, eo: _List_Nil, ag: "Log" });
    var $author$project$Interactive$InputString = { $: 0 };
    var $author$project$Interactive$string = $author$project$Interactive$InputString;
    var $elm$core$Result$map3 = F4(function(func, ra, rb, rc) {
      if (ra.$ === 1) {
        var x = ra.a;
        return $elm$core$Result$Err(x);
      } else {
        var a = ra.a;
        if (rb.$ === 1) {
          var x = rb.a;
          return $elm$core$Result$Err(x);
        } else {
          var b = rb.a;
          if (rc.$ === 1) {
            var x = rc.a;
            return $elm$core$Result$Err(x);
          } else {
            var c = rc.a;
            return $elm$core$Result$Ok(A3(func, a, b, c));
          }
        }
      }
    });
    var $mdgriffith$elm_codegen$Internal$Compiler$noImports = function(tipe) {
      return { ib: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, bM: tipe, c: _List_Nil };
    };
    var $mdgriffith$elm_codegen$Elm$Annotation$triple = F3(function(one, two, three) {
      return {
        ib: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, $mdgriffith$elm_codegen$Elm$Annotation$getAliases(one), $mdgriffith$elm_codegen$Elm$Annotation$getAliases(two)), $mdgriffith$elm_codegen$Elm$Annotation$getAliases(three)),
        bM: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled($mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(_List_fromArray([
          $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(one),
          $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(two),
          $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(three)
        ]))),
        c: _Utils_ap($mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(one), _Utils_ap($mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(two), $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(three)))
      };
    });
    var $mdgriffith$elm_codegen$Elm$triple = F3(function(oneExp, twoExp, threeExp) {
      return function(index) {
        var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, oneExp);
        var oneIndex = _v0.a;
        var one = _v0.b;
        var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, oneIndex, twoExp);
        var twoIndex = _v1.a;
        var two = _v1.b;
        var _v2 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, twoIndex, threeExp);
        var three = _v2.b;
        return {
          bM: A4($elm$core$Result$map3, F3(function(oneA, twoA, threeA) {
            return {
              ib: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, threeA.ib, A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, twoA.ib, oneA.ib)),
              e: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, threeA.e, A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, twoA.e, oneA.e)),
              hG: $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(A3($mdgriffith$elm_codegen$Elm$Annotation$triple, $mdgriffith$elm_codegen$Internal$Compiler$noImports(oneA.hG), $mdgriffith$elm_codegen$Internal$Compiler$noImports(twoA.hG), $mdgriffith$elm_codegen$Internal$Compiler$noImports(threeA.hG)))
            };
          }), one.bM, two.bM, three.bM),
          iH: $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression($mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(_List_fromArray([one.iH, two.iH, three.iH]))),
          c: _Utils_ap(one.c, _Utils_ap(two.c, three.c))
        };
      };
    });
    var $mdgriffith$elm_codegen$Elm$unit = function(_v0) {
      return {
        bM: $elm$core$Result$Ok($mdgriffith$elm_codegen$Internal$Compiler$inference($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit)),
        iH: $stil4m$elm_syntax$Elm$Syntax$Expression$UnitExpr,
        c: _List_Nil
      };
    };
    var $author$project$Example$Interactive$Build$buildArg = F4(function(options, context, namespace, target) {
      _v5$6:
        while (true) {
          _v5$14:
            while (true) {
              switch (target.$) {
                case 0:
                  if (target.a === "msg") {
                    return $elm$core$Result$Ok({ iu: context, c0: $author$project$Interactive$log });
                  } else {
                    var _var = target.a;
                    return $elm$core$Result$Err("I don't know how to build this type var " + _var);
                  }
                case 1:
                  var arg = target.a;
                  var result = target.b;
                  return $elm$core$Result$Err("Nested lambdas");
                case 2:
                  if (!target.a.b) {
                    return $elm$core$Result$Ok({ iu: context, c0: $mdgriffith$elm_codegen$Elm$unit });
                  } else {
                    if (target.a.b.b) {
                      if (!target.a.b.b.b) {
                        var _v6 = target.a;
                        var one = _v6.a;
                        var _v7 = _v6.b;
                        var two = _v7.a;
                        var _v8 = A4($author$project$Example$Interactive$Build$buildArg, options, context, namespace, one);
                        if (!_v8.$) {
                          var oneBuilt = _v8.a;
                          var _v9 = A4($author$project$Example$Interactive$Build$buildArg, options, oneBuilt.iu, namespace, two);
                          if (!_v9.$) {
                            var twoBuilt = _v9.a;
                            return $elm$core$Result$Ok({
                              iu: twoBuilt.iu,
                              c0: A2($mdgriffith$elm_codegen$Elm$tuple, oneBuilt.c0, twoBuilt.c0)
                            });
                          } else {
                            var err = _v9.a;
                            return $elm$core$Result$Err(err);
                          }
                        } else {
                          var err = _v8.a;
                          return $elm$core$Result$Err(err);
                        }
                      } else {
                        if (!target.a.b.b.b.b) {
                          var _v10 = target.a;
                          var one = _v10.a;
                          var _v11 = _v10.b;
                          var two = _v11.a;
                          var _v12 = _v11.b;
                          var three = _v12.a;
                          var _v13 = A4($author$project$Example$Interactive$Build$buildArg, options, context, namespace, one);
                          if (!_v13.$) {
                            var oneBuilt = _v13.a;
                            var _v14 = A4($author$project$Example$Interactive$Build$buildArg, options, oneBuilt.iu, namespace, two);
                            if (!_v14.$) {
                              var twoBuilt = _v14.a;
                              var _v15 = A4($author$project$Example$Interactive$Build$buildArg, options, twoBuilt.iu, namespace, three);
                              if (!_v15.$) {
                                var threeBuilt = _v15.a;
                                return $elm$core$Result$Ok({
                                  iu: threeBuilt.iu,
                                  c0: A3($mdgriffith$elm_codegen$Elm$triple, oneBuilt.c0, twoBuilt.c0, threeBuilt.c0)
                                });
                              } else {
                                var err = _v15.a;
                                return $elm$core$Result$Err(err);
                              }
                            } else {
                              var err = _v14.a;
                              return $elm$core$Result$Err(err);
                            }
                          } else {
                            var err = _v13.a;
                            return $elm$core$Result$Err(err);
                          }
                        } else {
                          break _v5$6;
                        }
                      }
                    } else {
                      break _v5$6;
                    }
                  }
                case 3:
                  if (!target.b.b) {
                    switch (target.a) {
                      case "String.String":
                        return $elm$core$Result$Ok(A3($author$project$Example$Interactive$Build$getVal, namespace, {
                          J: $mdgriffith$elm_codegen$Elm$string("Button"),
                          E: $author$project$Interactive$string
                        }, context));
                      case "Basics.Boolean":
                        return $elm$core$Result$Ok(A3($author$project$Example$Interactive$Build$getVal, namespace, {
                          J: $mdgriffith$elm_codegen$Elm$bool(true),
                          E: $author$project$Interactive$bool
                        }, context));
                      case "Basics.Int":
                        return $elm$core$Result$Ok(A3($author$project$Example$Interactive$Build$getVal, namespace, {
                          J: $mdgriffith$elm_codegen$Elm$int(1),
                          E: $author$project$Interactive$int
                        }, context));
                      case "Basics.Float":
                        return $elm$core$Result$Ok(A3($author$project$Example$Interactive$Build$getVal, namespace, {
                          J: $mdgriffith$elm_codegen$Elm$float(1),
                          E: $author$project$Interactive$float
                        }, context));
                      case "Basics.Bool":
                        return $elm$core$Result$Ok(A3($author$project$Example$Interactive$Build$getVal, namespace, {
                          J: $mdgriffith$elm_codegen$Elm$bool(true),
                          E: $author$project$Interactive$bool
                        }, context));
                      default:
                        break _v5$14;
                    }
                  } else {
                    if (!target.b.b.b) {
                      switch (target.a) {
                        case "Maybe.Maybe":
                          var _v16 = target.b;
                          var inner = _v16.a;
                          var _v17 = A4($author$project$Example$Interactive$Build$buildArg, options, context, namespace, inner);
                          if (_v17.$ === 1) {
                            var err = _v17.a;
                            return $elm$core$Result$Err(err);
                          } else {
                            var innerExample = _v17.a;
                            return $elm$core$Result$Ok({
                              iu: innerExample.iu,
                              c0: $mdgriffith$elm_codegen$Elm$just(innerExample.c0)
                            });
                          }
                        case "List.List":
                          var _v18 = target.b;
                          var inner = _v18.a;
                          var _v19 = A4($author$project$Example$Interactive$Build$buildArg, options, context, namespace, inner);
                          if (_v19.$ === 1) {
                            var err = _v19.a;
                            return $elm$core$Result$Err(err);
                          } else {
                            var innerExample = _v19.a;
                            return $elm$core$Result$Ok({
                              iu: innerExample.iu,
                              c0: $mdgriffith$elm_codegen$Elm$list(_List_fromArray([innerExample.c0]))
                            });
                          }
                        default:
                          break _v5$14;
                      }
                    } else {
                      break _v5$14;
                    }
                  }
                default:
                  var fields = target.a;
                  var maybeName = target.b;
                  var renderedResult = A3($elm$core$List$foldl, F2(function(_v29, gathered) {
                    var fieldName = _v29.a;
                    var fieldType = _v29.b;
                    if (gathered.$ === 1) {
                      var err2 = gathered.a;
                      return gathered;
                    } else {
                      var _v31 = gathered.a;
                      var currentContext = _v31.a;
                      var renderedFields = _v31.b;
                      var _v32 = A4($author$project$Example$Interactive$Build$buildArg, options, currentContext, fieldName, fieldType);
                      if (!_v32.$) {
                        var fieldExample = _v32.a;
                        return $elm$core$Result$Ok(_Utils_Tuple2(fieldExample.iu, A2($elm$core$List$cons, _Utils_Tuple2(fieldName, fieldExample.c0), renderedFields)));
                      } else {
                        var err2 = _v32.a;
                        return $elm$core$Result$Err(err2);
                      }
                    }
                  }), $elm$core$Result$Ok(_Utils_Tuple2(context, _List_Nil)), fields);
                  if (!renderedResult.$) {
                    var _v28 = renderedResult.a;
                    var newContext = _v28.a;
                    var rendered = _v28.b;
                    return $elm$core$Result$Ok({
                      iu: newContext,
                      c0: $mdgriffith$elm_codegen$Elm$record(rendered)
                    });
                  } else {
                    var err = renderedResult.a;
                    return $elm$core$Result$Err(err);
                  }
              }
            }
          var name = target.a;
          var vars = target.b;
          return A3($elm$core$List$foldl, F2(function(decl, buildResult) {
            if (!buildResult.$) {
              return buildResult;
            } else {
              if (A2($author$project$Example$Type$isCreatorOf, name, decl.ak)) {
                if (options.ab) {
                  var _v21 = A3($elm$core$List$foldl, F2(function(doc, untouched) {
                    var ctxt = untouched.a;
                    var existingBuilders = untouched.b;
                    var _v22 = A2($author$project$Example$Type$getBuilderOf, name, doc);
                    if (_v22.$ === 1) {
                      return untouched;
                    } else {
                      var builder = _v22.a;
                      var builtBuilderResult = A5($author$project$Example$Interactive$Build$buildBuilder, { ab: false }, ctxt, builder, builder.ak, _List_Nil);
                      if (builtBuilderResult.$ === 1) {
                        return untouched;
                      } else {
                        var builtBuilder = builtBuilderResult.a;
                        var builderSwitch = A3($author$project$Example$Interactive$Build$getValProtected, "includeBuilder", {
                          J: $mdgriffith$elm_codegen$Elm$bool(false),
                          E: $author$project$Interactive$bool
                        }, builtBuilder.iu);
                        return _Utils_Tuple2(builderSwitch.iu, A2($elm$core$List$cons, _Utils_Tuple2(builderSwitch.c0, builtBuilder.c0), existingBuilders));
                      }
                    }
                  }), _Utils_Tuple2(context, _List_Nil), context.F.jC);
                  var buildersContext = _v21.a;
                  var builders = _v21.b;
                  var exampleCall = A5($author$project$Example$Interactive$Build$buildExampleCall, { ab: false }, buildersContext, {
                    aT: function(_v25) {
                      return true;
                    },
                    z: decl
                  }, decl.ak, _List_Nil);
                  if (!exampleCall.$) {
                    var builtValue = exampleCall.a;
                    return $elm$core$Result$Ok({
                      iu: builtValue.iu,
                      c0: A3($elm$core$List$foldl, $author$project$Example$Interactive$Build$applyBuilder, builtValue.c0, builders)
                    });
                  } else {
                    var err2 = exampleCall.a;
                    return $elm$core$Result$Err(err2);
                  }
                } else {
                  return A5($author$project$Example$Interactive$Build$buildExampleCall, { ab: false }, context, {
                    aT: function(_v26) {
                      return true;
                    },
                    z: decl
                  }, decl.ak, _List_Nil);
                }
              } else {
                return buildResult;
              }
            }
          }), $elm$core$Result$Err("I don't know how to build a " + name), context.F.jC);
        }
      return $elm$core$Result$Err("I don't know how to build a tuple with values other than a 0, 2, and three.");
    });
    var $author$project$Example$Interactive$Build$buildBuilder = F5(function(options, context, originalValue, targetType, args) {
      buildBuilder:
        while (true) {
          if (targetType.$ === 1) {
            if (targetType.b.$ === 1) {
              var arg = targetType.a;
              var result = targetType.b;
              var _v4 = A4($author$project$Example$Interactive$Build$buildArg, options, context, originalValue.ag, arg);
              if (!_v4.$) {
                var argBuilt = _v4.a;
                var $temp$options = options, $temp$context = argBuilt.iu, $temp$originalValue = originalValue, $temp$targetType = result, $temp$args = A2($elm$core$List$cons, argBuilt.c0, args);
                options = $temp$options;
                context = $temp$context;
                originalValue = $temp$originalValue;
                targetType = $temp$targetType;
                args = $temp$args;
                continue buildBuilder;
              } else {
                var err = _v4.a;
                return $elm$core$Result$Err(err);
              }
            } else {
              var arg = targetType.a;
              var result = targetType.b;
              return $elm$core$Result$Ok({
                iu: context,
                c0: A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
                  bM: $elm$core$Maybe$Nothing,
                  eo: A2($elm$core$String$split, ".", context.F.ag),
                  ag: originalValue.ag
                }), $elm$core$List$reverse(args))
              });
            }
          } else {
            return A4($author$project$Example$Interactive$Build$buildArg, options, context, originalValue.ag, targetType);
          }
        }
    });
    var $author$project$Example$Interactive$Build$buildExampleCall = F5(function(options, context, bounds, targetType, args) {
      buildExampleCall:
        while (true) {
          if (targetType.$ === 1) {
            var arg = targetType.a;
            var result = targetType.b;
            var _v1 = A4($author$project$Example$Interactive$Build$buildArg, options, context, bounds.z.ag, arg);
            if (!_v1.$) {
              var argBuilt = _v1.a;
              if (result.$ === 1) {
                var $temp$options = options, $temp$context = argBuilt.iu, $temp$bounds = bounds, $temp$targetType = result, $temp$args = A2($elm$core$List$cons, argBuilt.c0, args);
                options = $temp$options;
                context = $temp$context;
                bounds = $temp$bounds;
                targetType = $temp$targetType;
                args = $temp$args;
                continue buildExampleCall;
              } else {
                return $elm$core$Result$Ok({
                  iu: argBuilt.iu,
                  c0: A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
                    bM: $elm$core$Maybe$Nothing,
                    eo: A2($elm$core$String$split, ".", argBuilt.iu.F.ag),
                    ag: bounds.z.ag
                  }), $elm$core$List$reverse(A2($elm$core$List$cons, argBuilt.c0, args)))
                });
              }
            } else {
              var err = _v1.a;
              return $elm$core$Result$Err(err);
            }
          } else {
            return $elm$core$Result$Ok({
              iu: context,
              c0: $mdgriffith$elm_codegen$Elm$value({
                bM: $elm$core$Maybe$Nothing,
                eo: A2($elm$core$String$split, ".", context.F.ag),
                ag: bounds.z.ag
              })
            });
          }
        }
    });
    var $author$project$Interactive$fromType = function(tipe) {
      _v0$4:
        while (true) {
          if (tipe.$ === 3 && !tipe.b.b) {
            switch (tipe.a) {
              case "String.String":
                return $elm$core$Maybe$Just({
                  J: $mdgriffith$elm_codegen$Elm$string(""),
                  E: $author$project$Interactive$InputString
                });
              case "Basics.Bool":
                return $elm$core$Maybe$Just({
                  J: $mdgriffith$elm_codegen$Elm$bool(false),
                  E: $author$project$Interactive$InputBool
                });
              case "Basics.Int":
                return $elm$core$Maybe$Just({
                  J: $mdgriffith$elm_codegen$Elm$int(0),
                  E: $author$project$Interactive$InputInt
                });
              case "Basics.Float":
                return $elm$core$Maybe$Just({
                  J: $mdgriffith$elm_codegen$Elm$float(0),
                  E: $author$project$Interactive$InputFloat
                });
              default:
                break _v0$4;
            }
          } else {
            break _v0$4;
          }
        }
      return $elm$core$Maybe$Nothing;
    };
    var $author$project$Example$Type$getArgsHelper = F2(function(tipe, found) {
      getArgsHelper:
        while (true) {
          if (tipe.$ === 1) {
            var arg = tipe.a;
            var result = tipe.b;
            var $temp$tipe = result, $temp$found = A2($elm$core$List$cons, arg, found);
            tipe = $temp$tipe;
            found = $temp$found;
            continue getArgsHelper;
          } else {
            return $elm$core$List$reverse(found);
          }
        }
    });
    var $author$project$Example$Type$getArgs = function(tipe) {
      return A2($author$project$Example$Type$getArgsHelper, tipe, _List_Nil);
    };
    var $mdgriffith$elm_codegen$Elm$Case$maybe = F2(function(mainExpression, branches) {
      return A3($mdgriffith$elm_codegen$Elm$Case$custom, mainExpression, $mdgriffith$elm_codegen$Elm$Annotation$maybe($mdgriffith$elm_codegen$Elm$Annotation$var("a")), _List_fromArray([
        A2($mdgriffith$elm_codegen$Elm$Case$branch, A2($mdgriffith$elm_codegen$Elm$Arg$customType, "Nothing", 0), function(_v0) {
          return branches.i5;
        }),
        A2($mdgriffith$elm_codegen$Elm$Case$branch, A2($mdgriffith$elm_codegen$Elm$Arg$item, $mdgriffith$elm_codegen$Elm$Arg$var(branches.iX.a), A2($mdgriffith$elm_codegen$Elm$Arg$customType, "Just", $elm$core$Basics$identity)), function(val) {
          return A2($elm$core$Tuple$second, branches.iX, val);
        })
      ]));
    });
    var $author$project$Interactive$InputMaybe = function(a) {
      return { $: 4, a };
    };
    var $author$project$Interactive$maybe = $author$project$Interactive$InputMaybe;
    var $author$project$Example$CallStack$name = function(_v0) {
      var call = _v0;
      return call.z.ag;
    };
    var $mdgriffith$elm_codegen$Elm$nothing = $mdgriffith$elm_codegen$Elm$maybe($elm$core$Maybe$Nothing);
    var $author$project$Example$CallStack$start = function(_v0) {
      var call = _v0;
      return call.z;
    };
    var $author$project$Example$Interactive$Build$buildHelper = F3(function(options, context, _v0) {
      var callstack = _v0;
      var starterCall = options.a9 ? A5($author$project$Example$Interactive$Build$buildBuilder, { ab: options.ab }, context, callstack.z, callstack.z.ak, _List_Nil) : A5($author$project$Example$Interactive$Build$buildExampleCall, { ab: options.ab }, context, {
        aT: function(_v7) {
          return true;
        },
        z: callstack.z
      }, callstack.z.ak, _List_Nil);
      if (!starterCall.$) {
        var call = starterCall.a;
        return A3($elm$core$List$foldl, F2(function(step, builtResult) {
          if (!builtResult.$) {
            var built = builtResult.a;
            if (step.be) {
              var _v3 = A3($author$project$Example$Interactive$Build$buildHelper, _Utils_update(options, { a9: true }), built.iu, step.bg);
              if (!_v3.$) {
                var builtStep = _v3.a;
                return $elm$core$Result$Ok({
                  iu: builtStep.iu,
                  c0: A2($mdgriffith$elm_codegen$Elm$Op$pipe, builtStep.c0, built.c0)
                });
              } else {
                var err2 = _v3.a;
                return $elm$core$Result$Err(err2);
              }
            } else {
              var _v4 = $author$project$Example$Type$getArgs($author$project$Example$CallStack$start(step.bg).ak);
              _v4$2:
                while (true) {
                  if (_v4.b) {
                    if (!_v4.b.b) {
                      var boolVal = A3($author$project$Example$Interactive$Build$getVal, $author$project$Example$CallStack$name(step.bg), {
                        J: $mdgriffith$elm_codegen$Elm$bool(false),
                        E: $author$project$Interactive$bool
                      }, built.iu);
                      return $elm$core$Result$Ok({
                        iu: boolVal.iu,
                        c0: A2($mdgriffith$elm_codegen$Elm$Op$pipe, A3($mdgriffith$elm_codegen$Elm$ifThen, boolVal.c0, $mdgriffith$elm_codegen$Elm$value({
                          bM: $elm$core$Maybe$Nothing,
                          eo: A2($elm$core$String$split, ".", context.F.ag),
                          ag: $author$project$Example$CallStack$name(step.bg)
                        }), $author$project$Example$Interactive$Build$genIdentity), built.c0)
                      });
                    } else {
                      if (!_v4.b.b.b) {
                        var argType = _v4.a;
                        var _v5 = _v4.b;
                        var _v6 = $author$project$Interactive$fromType(argType);
                        if (_v6.$ === 1) {
                          return builtResult;
                        } else {
                          var input = _v6.a;
                          var maybeVal = A3($author$project$Example$Interactive$Build$getVal, $author$project$Example$CallStack$name(step.bg), {
                            J: $mdgriffith$elm_codegen$Elm$nothing,
                            E: $author$project$Interactive$maybe(input.E)
                          }, built.iu);
                          return $elm$core$Result$Ok({
                            iu: maybeVal.iu,
                            c0: A2($mdgriffith$elm_codegen$Elm$Op$pipe, A2($mdgriffith$elm_codegen$Elm$Case$maybe, maybeVal.c0, {
                              iX: _Utils_Tuple2($author$project$Example$CallStack$name(step.bg) + "_option", function(val) {
                                return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
                                  bM: $elm$core$Maybe$Nothing,
                                  eo: A2($elm$core$String$split, ".", context.F.ag),
                                  ag: $author$project$Example$CallStack$name(step.bg)
                                }), _List_fromArray([val]));
                              }),
                              i5: $author$project$Example$Interactive$Build$genIdentity
                            }), built.c0)
                          });
                        }
                      } else {
                        break _v4$2;
                      }
                    }
                  } else {
                    break _v4$2;
                  }
                }
              return builtResult;
            }
          } else {
            var err2 = builtResult.a;
            return $elm$core$Result$Err(err2);
          }
        }), $elm$core$Result$Ok(call), $elm$core$List$reverse(callstack.aE));
      } else {
        var err = starterCall.a;
        return $elm$core$Result$Err(err);
      }
    });
    var $author$project$Example$Interactive$Build$initContext = function(modul) {
      return {
        P: 0,
        av: $mdgriffith$elm_codegen$Elm$value({ bM: $elm$core$Maybe$Nothing, eo: _List_Nil, ag: "model" }),
        F: modul,
        js: _List_Nil
      };
    };
    var $author$project$Example$Interactive$Build$build = F2(function(mod, callstack) {
      return A3($author$project$Example$Interactive$Build$buildHelper, { ab: true, a9: false }, $author$project$Example$Interactive$Build$initContext(mod), callstack);
    });
    var $author$project$Gen$Elm$apply = F2(function(applyArg_, applyArg_0) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
          A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
          $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
        ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
        eo: _List_fromArray(["Elm"]),
        ag: "apply"
      }), _List_fromArray([
        applyArg_,
        $mdgriffith$elm_codegen$Elm$list(applyArg_0)
      ]));
    });
    var $author$project$Example$Interactive$Rendered$genIdentity = A2($mdgriffith$elm_codegen$Elm$fn, $mdgriffith$elm_codegen$Elm$Arg$var("a"), function(a) {
      return a;
    });
    var $author$project$Example$Interactive$Rendered$applyBuilder = F2(function(_v0, value) {
      var includeBuilder = _v0.a;
      var builder = _v0.b;
      return A2($mdgriffith$elm_codegen$Elm$Op$pipe, A3($mdgriffith$elm_codegen$Elm$ifThen, includeBuilder, builder, $author$project$Example$Interactive$Rendered$genIdentity), value);
    });
    var $mdgriffith$elm_codegen$Elm$Annotation$char = A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_fromArray(["Char"]), "Char", _List_Nil);
    var $author$project$Gen$Elm$call_ = {
      bG: F2(function(aliasArg_, aliasArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "alias"
        }), _List_fromArray([aliasArg_, aliasArg_0]));
      }),
      bQ: F2(function(applyArg_, applyArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "apply"
        }), _List_fromArray([applyArg_, applyArg_0]));
      }),
      ij: function(boolArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$bool]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "bool"
        }), _List_fromArray([boolArg_]));
      },
      ca: function(charArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$char]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "char"
        }), _List_fromArray([charArg_]));
      },
      cp: function(commentArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "comment"
        }), _List_fromArray([commentArg_]));
      },
      cA: F2(function(customTypeArg_, customTypeArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Variant", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "customType"
        }), _List_fromArray([customTypeArg_, customTypeArg_0]));
      }),
      iw: F2(function(declarationArg_, declarationArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "declaration"
        }), _List_fromArray([declarationArg_, declarationArg_0]));
      }),
      aQ: function(docsArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("group", $mdgriffith$elm_codegen$Elm$Annotation$maybe($mdgriffith$elm_codegen$Elm$Annotation$string)),
              _Utils_Tuple2("members", $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string))
            ]))
          ]), $mdgriffith$elm_codegen$Elm$Annotation$string)),
          eo: _List_fromArray(["Elm"]),
          ag: "docs"
        }), _List_fromArray([docsArg_]));
      },
      ds: function(exposeArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "expose"
        }), _List_fromArray([exposeArg_]));
      },
      dt: F2(function(exposeWithArg_, exposeWithArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("exposeConstructor", $mdgriffith$elm_codegen$Elm$Annotation$bool),
              _Utils_Tuple2("group", $mdgriffith$elm_codegen$Elm$Annotation$maybe($mdgriffith$elm_codegen$Elm$Annotation$string))
            ])),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "exposeWith"
        }), _List_fromArray([exposeWithArg_, exposeWithArg_0]));
      }),
      as: F2(function(fileArg_, fileArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "File", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "file"
        }), _List_fromArray([fileArg_, fileArg_0]));
      }),
      dD: F3(function(fileWithArg_, fileWithArg_0, fileWithArg_1) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string),
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("docs", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
                  _Utils_Tuple2("group", $mdgriffith$elm_codegen$Elm$Annotation$maybe($mdgriffith$elm_codegen$Elm$Annotation$string)),
                  _Utils_Tuple2("members", $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string))
                ])))
              ]), $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string))),
              _Utils_Tuple2("aliases", $mdgriffith$elm_codegen$Elm$Annotation$list(A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string), $mdgriffith$elm_codegen$Elm$Annotation$string)))
            ])),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "File", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "fileWith"
        }), _List_fromArray([fileWithArg_, fileWithArg_0, fileWithArg_1]));
      }),
      iI: function(floatArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$float]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "float"
        }), _List_fromArray([floatArg_]));
      },
      dG: F2(function(fnArg_, fnArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "fn"
        }), _List_fromArray([fnArg_, fnArg_0]));
      }),
      dH: F3(function(fn2Arg_, fn2Arg_0, fn2Arg_1) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "fn2"
        }), _List_fromArray([fn2Arg_, fn2Arg_0, fn2Arg_1]));
      }),
      dI: F4(function(fn3Arg_, fn3Arg_0, fn3Arg_1, fn3Arg_2) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "fn3"
        }), _List_fromArray([fn3Arg_, fn3Arg_0, fn3Arg_1, fn3Arg_2]));
      }),
      dJ: F5(function(fn4Arg_, fn4Arg_0, fn4Arg_1, fn4Arg_2, fn4Arg_3) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "fn4"
        }), _List_fromArray([fn4Arg_, fn4Arg_0, fn4Arg_1, fn4Arg_2, fn4Arg_3]));
      }),
      dK: F6(function(fn5Arg_, fn5Arg_0, fn5Arg_1, fn5Arg_2, fn5Arg_3, fn5Arg_4) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "fn5"
        }), _List_fromArray([fn5Arg_, fn5Arg_0, fn5Arg_1, fn5Arg_2, fn5Arg_3, fn5Arg_4]));
      }),
      dL: F7(function(fn6Arg_, fn6Arg_0, fn6Arg_1, fn6Arg_2, fn6Arg_3, fn6Arg_4, fn6Arg_5) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "fn6"
        }), _List_fromArray([fn6Arg_, fn6Arg_0, fn6Arg_1, fn6Arg_2, fn6Arg_3, fn6Arg_4, fn6Arg_5]));
      }),
      dX: F2(function(functionArg_, functionArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil)))),
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "function"
        }), _List_fromArray([functionArg_, functionArg_0]));
      }),
      dY: F2(function(functionReducedArg_, functionReducedArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "functionReduced"
        }), _List_fromArray([functionReducedArg_, functionReducedArg_0]));
      }),
      d_: F2(function(getArg_, getArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "get"
        }), _List_fromArray([getArg_, getArg_0]));
      }),
      ec: function(hexArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$int]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "hex"
        }), _List_fromArray([hexArg_]));
      },
      el: F3(function(ifThenArg_, ifThenArg_0, ifThenArg_1) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "ifThen"
        }), _List_fromArray([ifThenArg_, ifThenArg_0, ifThenArg_1]));
      }),
      iT: function(intArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$int]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "int"
        }), _List_fromArray([intArg_]));
      },
      iX: function(justArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "just"
        }), _List_fromArray([justArg_]));
      },
      eO: function(listArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "list"
        }), _List_fromArray([listArg_]));
      },
      e6: function(maybeArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "maybe"
        }), _List_fromArray([maybeArg_]));
      },
      fZ: function(parseArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Result"]), "Result", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("declarations", $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil)))
            ]))
          ])))),
          eo: _List_fromArray(["Elm"]),
          ag: "parse"
        }), _List_fromArray([parseArg_]));
      },
      f5: F2(function(portIncomingArg_, portIncomingArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "portIncoming"
        }), _List_fromArray([portIncomingArg_, portIncomingArg_0]));
      }),
      f6: F2(function(portOutgoingArg_, portOutgoingArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "portOutgoing"
        }), _List_fromArray([portOutgoingArg_, portOutgoingArg_0]));
      }),
      gn: function(recordArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "record"
        }), _List_fromArray([recordArg_]));
      },
      jt: function(stringArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "string"
        }), _List_fromArray([stringArg_]));
      },
      hu: function(toStringArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
          ]), $mdgriffith$elm_codegen$Elm$Annotation$string)),
          eo: _List_fromArray(["Elm"]),
          ag: "toString"
        }), _List_fromArray([toStringArg_]));
      },
      hD: F3(function(tripleArg_, tripleArg_0, tripleArg_1) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "triple"
        }), _List_fromArray([tripleArg_, tripleArg_0, tripleArg_1]));
      }),
      hE: F2(function(tupleArg_, tupleArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "tuple"
        }), _List_fromArray([tupleArg_, tupleArg_0]));
      }),
      hM: function(unsafeArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "unsafe"
        }), _List_fromArray([unsafeArg_]));
      },
      hN: F3(function(unwrapArg_, unwrapArg_0, unwrapArg_1) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string),
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "unwrap"
        }), _List_fromArray([unwrapArg_, unwrapArg_0, unwrapArg_1]));
      }),
      hO: F2(function(unwrapperArg_, unwrapperArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string),
            $mdgriffith$elm_codegen$Elm$Annotation$string
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "unwrapper"
        }), _List_fromArray([unwrapperArg_, unwrapperArg_0]));
      }),
      hQ: F2(function(updateRecordArg_, updateRecordArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "updateRecord"
        }), _List_fromArray([updateRecordArg_, updateRecordArg_0]));
      }),
      hT: function(valArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "val"
        }), _List_fromArray([valArg_]));
      },
      hU: function(valueArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("importFrom", $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string)),
              _Utils_Tuple2("name", $mdgriffith$elm_codegen$Elm$Annotation$string),
              _Utils_Tuple2("annotation", $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil)))
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "value"
        }), _List_fromArray([valueArg_]));
      },
      bt: function(variantArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Variant", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "variant"
        }), _List_fromArray([variantArg_]));
      },
      hY: F2(function(variantWithArg_, variantWithArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Variant", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "variantWith"
        }), _List_fromArray([variantWithArg_, variantWithArg_0]));
      }),
      h4: F2(function(withDocumentationArg_, withDocumentationArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Declaration", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "withDocumentation"
        }), _List_fromArray([withDocumentationArg_, withDocumentationArg_0]));
      }),
      h6: F2(function(withTypeArg_, withTypeArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
          eo: _List_fromArray(["Elm"]),
          ag: "withType"
        }), _List_fromArray([withTypeArg_, withTypeArg_0]));
      })
    };
    var $author$project$Gen$Elm$nothing = $mdgriffith$elm_codegen$Elm$value({
      bM: $elm$core$Maybe$Just(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)),
      eo: _List_fromArray(["Elm"]),
      ag: "nothing"
    });
    var $author$project$Example$Interactive$Rendered$inputToLiteral = F2(function(input, exp) {
      switch (input.$) {
        case 0:
          return $author$project$Gen$Elm$call_.jt(exp);
        case 1:
          return $author$project$Gen$Elm$call_.ij(exp);
        case 2:
          return $author$project$Gen$Elm$call_.iT(exp);
        case 3:
          return $author$project$Gen$Elm$call_.iI(exp);
        default:
          var inner = input.a;
          return $author$project$Gen$Elm$nothing;
      }
    });
    var $author$project$Example$Interactive$Rendered$getVal = F3(function(nameBase, options, context) {
      var arg = nameBase;
      return {
        iu: _Utils_update(context, { P: context.P + 1 }),
        c0: A2($author$project$Example$Interactive$Rendered$inputToLiteral, options.E, A2($mdgriffith$elm_codegen$Elm$get, arg, context.av))
      };
    });
    var $author$project$Example$Interactive$Rendered$getValProtected = F3(function(nameBase, options, context) {
      var arg = _Utils_ap(nameBase, $elm$core$String$fromInt(context.P));
      return {
        iu: _Utils_update(context, {
          P: context.P + 1,
          js: A2($elm$core$List$cons, A2($author$project$Interactive$field, arg, options), context.js)
        }),
        c0: A2($mdgriffith$elm_codegen$Elm$get, arg, context.av)
      };
    });
    var $author$project$Gen$Elm$Op$pipe = F2(function(pipeArg_, pipeArg_0) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
          A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil),
          A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
        ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
        eo: _List_fromArray(["Elm", "Op"]),
        ag: "pipe"
      }), _List_fromArray([pipeArg_, pipeArg_0]));
    });
    var $author$project$Gen$Elm$record = function(recordArg_) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
          $mdgriffith$elm_codegen$Elm$Annotation$list(A2($mdgriffith$elm_codegen$Elm$Annotation$tuple, $mdgriffith$elm_codegen$Elm$Annotation$string, A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)))
        ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
        eo: _List_fromArray(["Elm"]),
        ag: "record"
      }), _List_fromArray([
        $mdgriffith$elm_codegen$Elm$list(recordArg_)
      ]));
    };
    var $author$project$Gen$Elm$value = function(valueArg_) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
          $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
            _Utils_Tuple2("importFrom", $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string)),
            _Utils_Tuple2("name", $mdgriffith$elm_codegen$Elm$Annotation$string),
            _Utils_Tuple2("annotation", $mdgriffith$elm_codegen$Elm$Annotation$maybe(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm", "Annotation"]), "Annotation", _List_Nil)))
          ]))
        ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil))),
        eo: _List_fromArray(["Elm"]),
        ag: "value"
      }), _List_fromArray([
        $mdgriffith$elm_codegen$Elm$record(_List_fromArray([
          A2($elm$core$Tuple$pair, "importFrom", $mdgriffith$elm_codegen$Elm$list(A2($elm$core$List$map, $mdgriffith$elm_codegen$Elm$string, valueArg_.eo))),
          A2($elm$core$Tuple$pair, "name", $mdgriffith$elm_codegen$Elm$string(valueArg_.ag)),
          A2($elm$core$Tuple$pair, "annotation", valueArg_.bM)
        ]))
      ]));
    };
    var $author$project$Example$Interactive$Rendered$buildArg = F4(function(options, context, namespace, target) {
      _v5$6:
        while (true) {
          _v5$13:
            while (true) {
              switch (target.$) {
                case 0:
                  if (target.a === "msg") {
                    return $elm$core$Result$Ok({
                      iu: context,
                      c0: $author$project$Gen$Elm$value({ bM: $mdgriffith$elm_codegen$Elm$nothing, eo: _List_Nil, ag: "Log" })
                    });
                  } else {
                    var _var = target.a;
                    return $elm$core$Result$Err("I don't know how to build a " + _var);
                  }
                case 1:
                  var arg = target.a;
                  var result = target.b;
                  return $elm$core$Result$Err("Nested lambdas");
                case 2:
                  if (!target.a.b) {
                    return $elm$core$Result$Ok({ iu: context, c0: $mdgriffith$elm_codegen$Elm$unit });
                  } else {
                    if (target.a.b.b) {
                      if (!target.a.b.b.b) {
                        var _v6 = target.a;
                        var one = _v6.a;
                        var _v7 = _v6.b;
                        var two = _v7.a;
                        var _v8 = A4($author$project$Example$Interactive$Rendered$buildArg, options, context, namespace, one);
                        if (!_v8.$) {
                          var oneBuilt = _v8.a;
                          var _v9 = A4($author$project$Example$Interactive$Rendered$buildArg, options, oneBuilt.iu, namespace, two);
                          if (!_v9.$) {
                            var twoBuilt = _v9.a;
                            return $elm$core$Result$Ok({
                              iu: twoBuilt.iu,
                              c0: A2($mdgriffith$elm_codegen$Elm$tuple, oneBuilt.c0, twoBuilt.c0)
                            });
                          } else {
                            var err = _v9.a;
                            return $elm$core$Result$Err(err);
                          }
                        } else {
                          var err = _v8.a;
                          return $elm$core$Result$Err(err);
                        }
                      } else {
                        if (!target.a.b.b.b.b) {
                          var _v10 = target.a;
                          var one = _v10.a;
                          var _v11 = _v10.b;
                          var two = _v11.a;
                          var _v12 = _v11.b;
                          var three = _v12.a;
                          var _v13 = A4($author$project$Example$Interactive$Rendered$buildArg, options, context, namespace, one);
                          if (!_v13.$) {
                            var oneBuilt = _v13.a;
                            var _v14 = A4($author$project$Example$Interactive$Rendered$buildArg, options, oneBuilt.iu, namespace, two);
                            if (!_v14.$) {
                              var twoBuilt = _v14.a;
                              var _v15 = A4($author$project$Example$Interactive$Rendered$buildArg, options, twoBuilt.iu, namespace, three);
                              if (!_v15.$) {
                                var threeBuilt = _v15.a;
                                return $elm$core$Result$Ok({
                                  iu: threeBuilt.iu,
                                  c0: A3($mdgriffith$elm_codegen$Elm$triple, oneBuilt.c0, twoBuilt.c0, threeBuilt.c0)
                                });
                              } else {
                                var err = _v15.a;
                                return $elm$core$Result$Err(err);
                              }
                            } else {
                              var err = _v14.a;
                              return $elm$core$Result$Err(err);
                            }
                          } else {
                            var err = _v13.a;
                            return $elm$core$Result$Err(err);
                          }
                        } else {
                          break _v5$6;
                        }
                      }
                    } else {
                      break _v5$6;
                    }
                  }
                case 3:
                  if (!target.b.b) {
                    switch (target.a) {
                      case "String.String":
                        return $elm$core$Result$Ok(A3($author$project$Example$Interactive$Rendered$getVal, namespace, {
                          J: $mdgriffith$elm_codegen$Elm$string(""),
                          E: $author$project$Interactive$string
                        }, context));
                      case "Basics.Bool":
                        return $elm$core$Result$Ok(A3($author$project$Example$Interactive$Rendered$getVal, namespace, {
                          J: $mdgriffith$elm_codegen$Elm$bool(true),
                          E: $author$project$Interactive$bool
                        }, context));
                      case "Basics.Int":
                        return $elm$core$Result$Ok(A3($author$project$Example$Interactive$Rendered$getVal, namespace, {
                          J: $mdgriffith$elm_codegen$Elm$int(1),
                          E: $author$project$Interactive$int
                        }, context));
                      case "Basics.Float":
                        return $elm$core$Result$Ok(A3($author$project$Example$Interactive$Rendered$getVal, namespace, {
                          J: $mdgriffith$elm_codegen$Elm$float(1),
                          E: $author$project$Interactive$float
                        }, context));
                      default:
                        break _v5$13;
                    }
                  } else {
                    if (!target.b.b.b) {
                      switch (target.a) {
                        case "Maybe.Maybe":
                          var _v16 = target.b;
                          var inner = _v16.a;
                          var _v17 = A4($author$project$Example$Interactive$Rendered$buildArg, options, context, namespace, inner);
                          if (_v17.$ === 1) {
                            var err = _v17.a;
                            return $elm$core$Result$Err(err);
                          } else {
                            var innerExample = _v17.a;
                            return $elm$core$Result$Ok({
                              iu: innerExample.iu,
                              c0: $mdgriffith$elm_codegen$Elm$just(innerExample.c0)
                            });
                          }
                        case "List.List":
                          var _v18 = target.b;
                          var inner = _v18.a;
                          var _v19 = A4($author$project$Example$Interactive$Rendered$buildArg, options, context, namespace, inner);
                          if (_v19.$ === 1) {
                            var err = _v19.a;
                            return $elm$core$Result$Err(err);
                          } else {
                            var innerExample = _v19.a;
                            return $elm$core$Result$Ok({
                              iu: innerExample.iu,
                              c0: $mdgriffith$elm_codegen$Elm$list(_List_fromArray([innerExample.c0]))
                            });
                          }
                        default:
                          break _v5$13;
                      }
                    } else {
                      break _v5$13;
                    }
                  }
                default:
                  var fields = target.a;
                  var maybeName = target.b;
                  var renderedResult = A3($elm$core$List$foldl, F2(function(_v29, gathered) {
                    var fieldName = _v29.a;
                    var fieldType = _v29.b;
                    if (gathered.$ === 1) {
                      var err2 = gathered.a;
                      return gathered;
                    } else {
                      var _v31 = gathered.a;
                      var currentContext = _v31.a;
                      var renderedFields = _v31.b;
                      var _v32 = A4($author$project$Example$Interactive$Rendered$buildArg, options, currentContext, fieldName, fieldType);
                      if (!_v32.$) {
                        var fieldExample = _v32.a;
                        return $elm$core$Result$Ok(_Utils_Tuple2(fieldExample.iu, A2($elm$core$List$cons, A2($mdgriffith$elm_codegen$Elm$tuple, $mdgriffith$elm_codegen$Elm$string(fieldName), fieldExample.c0), renderedFields)));
                      } else {
                        var err2 = _v32.a;
                        return $elm$core$Result$Err(err2);
                      }
                    }
                  }), $elm$core$Result$Ok(_Utils_Tuple2(context, _List_Nil)), fields);
                  if (!renderedResult.$) {
                    var _v28 = renderedResult.a;
                    var newContext = _v28.a;
                    var rendered = _v28.b;
                    return $elm$core$Result$Ok({
                      iu: newContext,
                      c0: $author$project$Gen$Elm$record(rendered)
                    });
                  } else {
                    var err = renderedResult.a;
                    return $elm$core$Result$Err(err);
                  }
              }
            }
          var name = target.a;
          var vars = target.b;
          return A3($elm$core$List$foldl, F2(function(decl, buildResult) {
            if (!buildResult.$) {
              return buildResult;
            } else {
              if (A2($author$project$Example$Type$isCreatorOf, name, decl.ak)) {
                if (options.ab) {
                  var exampleCall = A5($author$project$Example$Interactive$Rendered$buildExampleCall, { ab: false }, context, {
                    aT: function(_v25) {
                      return true;
                    },
                    z: decl
                  }, decl.ak, _List_Nil);
                  if (!exampleCall.$) {
                    var builtValue = exampleCall.a;
                    var _v22 = A3($elm$core$List$foldl, F2(function(doc, untouched) {
                      var ctxt = untouched.a;
                      var existingBuilders = untouched.b;
                      var _v23 = A2($author$project$Example$Type$getBuilderOf, name, doc);
                      if (_v23.$ === 1) {
                        return untouched;
                      } else {
                        var builder = _v23.a;
                        var builtBuilderResult = A5($author$project$Example$Interactive$Rendered$buildBuilder, { ab: false }, ctxt, builder, builder.ak, _List_Nil);
                        if (builtBuilderResult.$ === 1) {
                          return untouched;
                        } else {
                          var builtBuilder = builtBuilderResult.a;
                          var builderSwitch = A3($author$project$Example$Interactive$Rendered$getValProtected, "includeBuilder", {
                            J: $mdgriffith$elm_codegen$Elm$bool(false),
                            E: $author$project$Interactive$bool
                          }, builtBuilder.iu);
                          return _Utils_Tuple2(builderSwitch.iu, A2($elm$core$List$cons, _Utils_Tuple2(builderSwitch.c0, builtBuilder.c0), existingBuilders));
                        }
                      }
                    }), _Utils_Tuple2(builtValue.iu, _List_Nil), context.F.jC);
                    var buildersContext = _v22.a;
                    var builders = _v22.b;
                    return $elm$core$Result$Ok({
                      iu: buildersContext,
                      c0: A3($elm$core$List$foldl, $author$project$Example$Interactive$Rendered$applyBuilder, builtValue.c0, builders)
                    });
                  } else {
                    var err2 = exampleCall.a;
                    return $elm$core$Result$Err(err2);
                  }
                } else {
                  return A5($author$project$Example$Interactive$Rendered$buildExampleCall, { ab: false }, context, {
                    aT: function(_v26) {
                      return true;
                    },
                    z: decl
                  }, decl.ak, _List_Nil);
                }
              } else {
                return buildResult;
              }
            }
          }), $elm$core$Result$Err("I don't know how to build a " + name), context.F.jC);
        }
      return $elm$core$Result$Err("I don't know how to build a tuple with values other than a 0, 2, and three.");
    });
    var $author$project$Example$Interactive$Rendered$buildBuilder = F5(function(options, context, originalValue, targetType, args) {
      buildBuilder:
        while (true) {
          if (targetType.$ === 1) {
            if (targetType.b.$ === 1) {
              var arg = targetType.a;
              var result = targetType.b;
              var _v4 = A4($author$project$Example$Interactive$Rendered$buildArg, options, context, originalValue.ag, arg);
              if (!_v4.$) {
                var argBuilt = _v4.a;
                var $temp$options = options, $temp$context = argBuilt.iu, $temp$originalValue = originalValue, $temp$targetType = result, $temp$args = A2($elm$core$List$cons, argBuilt.c0, args);
                options = $temp$options;
                context = $temp$context;
                originalValue = $temp$originalValue;
                targetType = $temp$targetType;
                args = $temp$args;
                continue buildBuilder;
              } else {
                var err = _v4.a;
                return $elm$core$Result$Err(err);
              }
            } else {
              var arg = targetType.a;
              var result = targetType.b;
              return $elm$core$Result$Ok({
                iu: context,
                c0: A2($mdgriffith$elm_codegen$Elm$fn, $mdgriffith$elm_codegen$Elm$Arg$var("a"), $author$project$Gen$Elm$Op$pipe(A2($author$project$Gen$Elm$apply, $author$project$Gen$Elm$value({
                  bM: $mdgriffith$elm_codegen$Elm$nothing,
                  eo: A2($elm$core$String$split, ".", context.F.ag),
                  ag: originalValue.ag
                }), $elm$core$List$reverse(args))))
              });
            }
          } else {
            return A4($author$project$Example$Interactive$Rendered$buildArg, options, context, originalValue.ag, targetType);
          }
        }
    });
    var $author$project$Example$Interactive$Rendered$buildExampleCall = F5(function(options, context, bounds, targetType, args) {
      buildExampleCall:
        while (true) {
          if (targetType.$ === 1) {
            var arg = targetType.a;
            var result = targetType.b;
            var _v1 = A4($author$project$Example$Interactive$Rendered$buildArg, options, context, bounds.z.ag, arg);
            if (!_v1.$) {
              var argBuilt = _v1.a;
              if (result.$ === 1) {
                var $temp$options = options, $temp$context = argBuilt.iu, $temp$bounds = bounds, $temp$targetType = result, $temp$args = A2($elm$core$List$cons, argBuilt.c0, args);
                options = $temp$options;
                context = $temp$context;
                bounds = $temp$bounds;
                targetType = $temp$targetType;
                args = $temp$args;
                continue buildExampleCall;
              } else {
                return $elm$core$Result$Ok({
                  iu: argBuilt.iu,
                  c0: A2($author$project$Gen$Elm$apply, $author$project$Gen$Elm$value({
                    bM: $mdgriffith$elm_codegen$Elm$nothing,
                    eo: A2($elm$core$String$split, ".", argBuilt.iu.F.ag),
                    ag: bounds.z.ag
                  }), $elm$core$List$reverse(A2($elm$core$List$cons, argBuilt.c0, args)))
                });
              }
            } else {
              var err = _v1.a;
              return $elm$core$Result$Err(err);
            }
          } else {
            return $elm$core$Result$Ok({
              iu: context,
              c0: $author$project$Gen$Elm$value({
                bM: $mdgriffith$elm_codegen$Elm$nothing,
                eo: A2($elm$core$String$split, ".", context.F.ag),
                ag: bounds.z.ag
              })
            });
          }
        }
    });
    var $author$project$Example$Interactive$Rendered$buildHelper = F3(function(options, context, _v0) {
      var callstack = _v0;
      var starterCall = options.a9 ? A5($author$project$Example$Interactive$Rendered$buildBuilder, { ab: options.ab }, context, callstack.z, callstack.z.ak, _List_Nil) : A5($author$project$Example$Interactive$Rendered$buildExampleCall, { ab: options.ab }, context, {
        aT: function(_v7) {
          return true;
        },
        z: callstack.z
      }, callstack.z.ak, _List_Nil);
      if (!starterCall.$) {
        var call = starterCall.a;
        return A3($elm$core$List$foldl, F2(function(step, builtResult) {
          if (!builtResult.$) {
            var built = builtResult.a;
            if (step.be) {
              var _v3 = A3($author$project$Example$Interactive$Rendered$buildHelper, _Utils_update(options, { a9: true }), built.iu, step.bg);
              if (!_v3.$) {
                var builtStep = _v3.a;
                return $elm$core$Result$Ok({
                  iu: builtStep.iu,
                  c0: A2($mdgriffith$elm_codegen$Elm$Op$pipe, builtStep.c0, built.c0)
                });
              } else {
                var err2 = _v3.a;
                return $elm$core$Result$Err(err2);
              }
            } else {
              var _v4 = $author$project$Example$Type$getArgs($author$project$Example$CallStack$start(step.bg).ak);
              _v4$2:
                while (true) {
                  if (_v4.b) {
                    if (!_v4.b.b) {
                      var boolVal = A2($mdgriffith$elm_codegen$Elm$get, $author$project$Example$CallStack$name(step.bg), built.iu.av);
                      return $elm$core$Result$Ok({
                        iu: built.iu,
                        c0: A2($mdgriffith$elm_codegen$Elm$Op$pipe, A3($mdgriffith$elm_codegen$Elm$ifThen, boolVal, A2($mdgriffith$elm_codegen$Elm$fn, $mdgriffith$elm_codegen$Elm$Arg$var("a"), $author$project$Gen$Elm$Op$pipe(A2($author$project$Gen$Elm$apply, $author$project$Gen$Elm$value({
                          bM: $mdgriffith$elm_codegen$Elm$nothing,
                          eo: A2($elm$core$String$split, ".", context.F.ag),
                          ag: $author$project$Example$CallStack$name(step.bg)
                        }), _List_Nil))), $author$project$Example$Interactive$Rendered$genIdentity), built.c0)
                      });
                    } else {
                      if (!_v4.b.b.b) {
                        var argType = _v4.a;
                        var _v5 = _v4.b;
                        var _v6 = $author$project$Interactive$fromType(argType);
                        if (_v6.$ === 1) {
                          return builtResult;
                        } else {
                          var input = _v6.a;
                          var maybeVal = A2($mdgriffith$elm_codegen$Elm$get, $author$project$Example$CallStack$name(step.bg), built.iu.av);
                          return $elm$core$Result$Ok({
                            iu: built.iu,
                            c0: A2($mdgriffith$elm_codegen$Elm$Op$pipe, A2($mdgriffith$elm_codegen$Elm$Case$maybe, maybeVal, {
                              iX: _Utils_Tuple2($author$project$Example$CallStack$name(step.bg) + "_option", function(val) {
                                return A2($mdgriffith$elm_codegen$Elm$fn, $mdgriffith$elm_codegen$Elm$Arg$var("a"), $author$project$Gen$Elm$Op$pipe(A2($author$project$Gen$Elm$apply, $author$project$Gen$Elm$value({
                                  bM: $mdgriffith$elm_codegen$Elm$nothing,
                                  eo: A2($elm$core$String$split, ".", context.F.ag),
                                  ag: $author$project$Example$CallStack$name(step.bg)
                                }), _List_fromArray([
                                  A2($author$project$Example$Interactive$Rendered$inputToLiteral, input.E, val)
                                ]))));
                              }),
                              i5: $author$project$Example$Interactive$Rendered$genIdentity
                            }), built.c0)
                          });
                        }
                      } else {
                        break _v4$2;
                      }
                    }
                  } else {
                    break _v4$2;
                  }
                }
              return builtResult;
            }
          } else {
            var err2 = builtResult.a;
            return $elm$core$Result$Err(err2);
          }
        }), $elm$core$Result$Ok(call), $elm$core$List$reverse(callstack.aE));
      } else {
        var err = starterCall.a;
        return $elm$core$Result$Err(err);
      }
    });
    var $author$project$Example$Interactive$Rendered$initContext = function(modul) {
      return {
        P: 0,
        av: $mdgriffith$elm_codegen$Elm$value({ bM: $elm$core$Maybe$Nothing, eo: _List_Nil, ag: "model" }),
        F: modul,
        js: _List_Nil
      };
    };
    var $author$project$Gen$Elm$toString = function(toStringArg_) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
          A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Elm"]), "Expression", _List_Nil)
        ]), $mdgriffith$elm_codegen$Elm$Annotation$string)),
        eo: _List_fromArray(["Elm"]),
        ag: "toString"
      }), _List_fromArray([toStringArg_]));
    };
    var $author$project$Example$Interactive$Rendered$build = F2(function(mod, callstack) {
      return A2($elm$core$Result$map, function(ok) {
        return _Utils_update(ok, {
          c0: $author$project$Gen$Elm$toString(ok.c0)
        });
      }, A3($author$project$Example$Interactive$Rendered$buildHelper, { ab: true, a9: false }, $author$project$Example$Interactive$Rendered$initContext(mod), callstack));
    });
    var $author$project$Example$CallStack$CallStack = $elm$core$Basics$identity;
    var $author$project$Example$Type$getResultType = function(tipe) {
      getResultType:
        while (true) {
          if (tipe.$ === 1) {
            var result = tipe.b;
            var $temp$tipe = result;
            tipe = $temp$tipe;
            continue getResultType;
          } else {
            return tipe;
          }
        }
    };
    var $author$project$Example$Type$listMatches = F2(function(ones, twos) {
      listMatches:
        while (true) {
          var _v6 = _Utils_Tuple2(ones, twos);
          _v6$2:
            while (true) {
              if (!_v6.a.b) {
                if (!_v6.b.b) {
                  return true;
                } else {
                  break _v6$2;
                }
              } else {
                if (_v6.b.b) {
                  var _v7 = _v6.a;
                  var oneTop = _v7.a;
                  var oneRemain = _v7.b;
                  var _v8 = _v6.b;
                  var twoTop = _v8.a;
                  var twoRemain = _v8.b;
                  if (A2($author$project$Example$Type$matches, oneTop, twoTop)) {
                    var $temp$ones = oneRemain, $temp$twos = twoRemain;
                    ones = $temp$ones;
                    twos = $temp$twos;
                    continue listMatches;
                  } else {
                    return false;
                  }
                } else {
                  break _v6$2;
                }
              }
            }
          return false;
        }
    });
    var $author$project$Example$Type$matches = F2(function(one, two) {
      switch (one.$) {
        case 0:
          var oneVar = one.a;
          if (!two.$) {
            var twoVar = two.a;
            return true;
          } else {
            return false;
          }
        case 1:
          var fst = one.a;
          var snd = one.b;
          if (two.$ === 1) {
            var twoFst = two.a;
            var twoSnd = two.b;
            return A2($author$project$Example$Type$matches, fst, twoFst) && A2($author$project$Example$Type$matches, snd, twoSnd);
          } else {
            return false;
          }
        case 2:
          var ones = one.a;
          if (two.$ === 2) {
            var twos = two.a;
            return A2($author$project$Example$Type$listMatches, ones, twos);
          } else {
            return false;
          }
        case 3:
          var name = one.a;
          var types = one.b;
          if (two.$ === 3) {
            var twoName = two.a;
            var twoTypes = two.b;
            return _Utils_eq(name, twoName) && A2($author$project$Example$Type$listMatches, types, twoTypes);
          } else {
            return false;
          }
        default:
          var fields = one.a;
          var maybeExtensible = one.b;
          if (two.$ === 4) {
            var twoRecords = two.a;
            var twoMaybeExtensible = two.b;
            return false;
          } else {
            return false;
          }
      }
    });
    var $author$project$Example$Type$isBuilderOf = F2(function(desiredResult, possibleBuilder) {
      isBuilderOf:
        while (true) {
          if (possibleBuilder.$ === 1) {
            var arg = possibleBuilder.a;
            var result = possibleBuilder.b;
            if (A2($author$project$Example$Type$matches, desiredResult, arg) && A2($author$project$Example$Type$matches, desiredResult, result)) {
              return true;
            } else {
              var $temp$desiredResult = desiredResult, $temp$possibleBuilder = result;
              desiredResult = $temp$desiredResult;
              possibleBuilder = $temp$possibleBuilder;
              continue isBuilderOf;
            }
          } else {
            return false;
          }
        }
    });
    var $author$project$Example$CallStack$matchesResultType = F2(function(one, two) {
      return A2($author$project$Example$Type$matches, $author$project$Example$Type$getResultType(one.ak), $author$project$Example$Type$getResultType(two.ak));
    });
    var $author$project$Example$CallStack$mergeCallStacks = F2(function(maybeOne, maybeTwo) {
      var _v0 = _Utils_Tuple2(maybeOne, maybeTwo);
      if (!_v0.a.$) {
        if (!_v0.b.$) {
          var one = _v0.a.a;
          var two = _v0.b.a;
          return $elm$core$Maybe$Just(_Utils_ap(one, two));
        } else {
          var one = _v0.a.a;
          var _v1 = _v0.b;
          return $elm$core$Maybe$Just(one);
        }
      } else {
        if (!_v0.b.$) {
          var _v2 = _v0.a;
          var two = _v0.b.a;
          return $elm$core$Maybe$Just(two);
        } else {
          var _v3 = _v0.a;
          var _v4 = _v0.b;
          return $elm$core$Maybe$Nothing;
        }
      }
    });
    var $author$project$Example$CallStack$singleCall = function(val) {
      return {
        ai: $author$project$Example$Type$getResultType(val.ak),
        z: val,
        aE: _List_Nil
      };
    };
    var $author$project$Example$CallStack$find = F3(function(inScope, built, targeting) {
      var resultType = $author$project$Example$Type$getResultType(targeting.z.ak);
      return targeting.aT(resultType) ? $elm$core$Maybe$Just(_List_fromArray([
        {
          ai: resultType,
          z: targeting.z,
          aE: A2($elm$core$List$filterMap, function(val) {
            return A2($author$project$Example$Type$isBuilderOf, resultType, val.ak) ? $elm$core$Maybe$Just({
              be: false,
              bg: $author$project$Example$CallStack$singleCall(val)
            }) : $elm$core$Maybe$Nothing;
          }, inScope)
        }
      ])) : A3($elm$core$List$foldl, F2(function(val, gathered) {
        if (A2($author$project$Example$CallStack$matchesResultType, val, targeting.z)) {
          return gathered;
        } else {
          if (A2($elm$core$List$any, $author$project$Example$CallStack$matchesResultType(val), built)) {
            return gathered;
          } else {
            var maybeSubCallStacks = A3($author$project$Example$CallStack$find, inScope, A2($elm$core$List$cons, targeting.z, built), { aT: targeting.aT, z: val });
            if (maybeSubCallStacks.$ === 1) {
              return gathered;
            } else {
              var subCallStacks = maybeSubCallStacks.a;
              return A2($author$project$Example$CallStack$mergeCallStacks, gathered, $elm$core$Maybe$Just(A2($elm$core$List$map, function(subCall) {
                var subCallDetails = subCall;
                var optionalBuilders = A2($elm$core$List$filterMap, function(optionalVal) {
                  return A2($author$project$Example$Type$isBuilderOf, resultType, optionalVal.ak) ? $elm$core$Maybe$Just({
                    be: false,
                    bg: $author$project$Example$CallStack$singleCall(optionalVal)
                  }) : $elm$core$Maybe$Nothing;
                }, inScope);
                return {
                  ai: subCallDetails.ai,
                  z: targeting.z,
                  aE: A2($elm$core$List$cons, {
                    be: true,
                    bg: $author$project$Example$CallStack$singleCall(subCallDetails.z)
                  }, _Utils_ap(optionalBuilders, subCallDetails.aE))
                };
              }, subCallStacks)));
            }
          }
        }
      }), $elm$core$Maybe$Nothing, inScope);
    });
    var $author$project$Example$Interactive$getErrorString = function(result) {
      if (!result.$) {
        return "Ok";
      } else {
        var err = result.a;
        return err;
      }
    };
    var $author$project$Example$CallStack$getResultType = function(_v0) {
      var call = _v0;
      return call.ai;
    };
    var $author$project$Example$Interactive$runnerEnd = F2(function(runners, target) {
      runnerEnd:
        while (true) {
          if (!runners.b) {
            return false;
          } else {
            var runner = runners.a;
            var remain = runners.b;
            if (runner.aN(target)) {
              return true;
            } else {
              var $temp$runners = remain, $temp$target = target;
              runners = $temp$runners;
              target = $temp$target;
              continue runnerEnd;
            }
          }
        }
    });
    var $author$project$Example$Interactive$buildExampleCallStack = F2(function(mod, bounds) {
      var options = {
        aT: $author$project$Example$Interactive$runnerEnd(bounds.gC),
        z: bounds.z
      };
      var _v0 = A3($author$project$Example$CallStack$find, mod.jC, _List_Nil, options);
      if (_v0.$ === 1) {
        return $elm$core$Result$Err("Nothing way to build desired type `" + (options.z.ag + "`"));
      } else {
        if (!_v0.a.b) {
          return $elm$core$Result$Err("No way to build desired type `" + (options.z.ag + "`"));
        } else {
          var _v1 = _v0.a;
          var callStack = _v1.a;
          var calls = _v1.b;
          var renderedResult = A2($author$project$Example$Interactive$Rendered$build, mod, callStack);
          var exampleResult = A2($author$project$Example$Interactive$Build$build, mod, callStack);
          var _v2 = _Utils_Tuple2(renderedResult, exampleResult);
          if (!_v2.a.$ && !_v2.b.$) {
            var rendered = _v2.a.a;
            var example = _v2.b.a;
            return $elm$core$Result$Ok({
              df: rendered,
              bc: example,
              gt: $author$project$Example$CallStack$getResultType(callStack)
            });
          } else {
            var err1 = _v2.a;
            var err2 = _v2.b;
            return $elm$core$Result$Err(`Something terribly terribly wrong happened

rendered: ` + ($author$project$Example$Interactive$getErrorString(err1) + (`

example: ` + $author$project$Example$Interactive$getErrorString(err2))));
          }
        }
      }
    });
    var $author$project$Gen$Html$call_ = {
      by: F2(function(aArg_, aArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "a"
        }), _List_fromArray([aArg_, aArg_0]));
      }),
      bz: F2(function(abbrArg_, abbrArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "abbr"
        }), _List_fromArray([abbrArg_, abbrArg_0]));
      }),
      bF: F2(function(addressArg_, addressArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "address"
        }), _List_fromArray([addressArg_, addressArg_0]));
      }),
      bS: F2(function(articleArg_, articleArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "article"
        }), _List_fromArray([articleArg_, articleArg_0]));
      }),
      bT: F2(function(asideArg_, asideArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "aside"
        }), _List_fromArray([asideArg_, asideArg_0]));
      }),
      bV: F2(function(audioArg_, audioArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "audio"
        }), _List_fromArray([audioArg_, audioArg_0]));
      }),
      bZ: F2(function(bArg_, bArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "b"
        }), _List_fromArray([bArg_, bArg_0]));
      }),
      b1: F2(function(bdiArg_, bdiArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "bdi"
        }), _List_fromArray([bdiArg_, bdiArg_0]));
      }),
      b2: F2(function(bdoArg_, bdoArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "bdo"
        }), _List_fromArray([bdoArg_, bdoArg_0]));
      }),
      b4: F2(function(blockquoteArg_, blockquoteArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "blockquote"
        }), _List_fromArray([blockquoteArg_, blockquoteArg_0]));
      }),
      b5: F2(function(brArg_, brArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "br"
        }), _List_fromArray([brArg_, brArg_0]));
      }),
      b6: F2(function(buttonArg_, buttonArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "button"
        }), _List_fromArray([buttonArg_, buttonArg_0]));
      }),
      b7: F2(function(canvasArg_, canvasArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "canvas"
        }), _List_fromArray([canvasArg_, canvasArg_0]));
      }),
      b8: F2(function(captionArg_, captionArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "caption"
        }), _List_fromArray([captionArg_, captionArg_0]));
      }),
      ch: F2(function(citeArg_, citeArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "cite"
        }), _List_fromArray([citeArg_, citeArg_0]));
      }),
      ck: F2(function(codeArg_, codeArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "code"
        }), _List_fromArray([codeArg_, codeArg_0]));
      }),
      cl: F2(function(colArg_, colArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "col"
        }), _List_fromArray([colArg_, colArg_0]));
      }),
      cm: F2(function(colgroupArg_, colgroupArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "colgroup"
        }), _List_fromArray([colgroupArg_, colgroupArg_0]));
      }),
      cC: F2(function(datalistArg_, datalistArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "datalist"
        }), _List_fromArray([datalistArg_, datalistArg_0]));
      }),
      cF: F2(function(ddArg_, ddArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "dd"
        }), _List_fromArray([ddArg_, ddArg_0]));
      }),
      cL: F2(function(delArg_, delArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "del"
        }), _List_fromArray([delArg_, delArg_0]));
      }),
      cQ: F2(function(detailsArg_, detailsArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "details"
        }), _List_fromArray([detailsArg_, detailsArg_0]));
      }),
      cR: F2(function(dfnArg_, dfnArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "dfn"
        }), _List_fromArray([dfnArg_, dfnArg_0]));
      }),
      cW: F2(function(divArg_, divArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "div"
        }), _List_fromArray([divArg_, divArg_0]));
      }),
      cY: F2(function(dlArg_, dlArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "dl"
        }), _List_fromArray([dlArg_, dlArg_0]));
      }),
      c4: F2(function(dtArg_, dtArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "dt"
        }), _List_fromArray([dtArg_, dtArg_0]));
      }),
      c6: F2(function(emArg_, emArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "em"
        }), _List_fromArray([emArg_, emArg_0]));
      }),
      c7: F2(function(embedArg_, embedArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "embed"
        }), _List_fromArray([embedArg_, embedArg_0]));
      }),
      dA: F2(function(fieldsetArg_, fieldsetArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "fieldset"
        }), _List_fromArray([fieldsetArg_, fieldsetArg_0]));
      }),
      dB: F2(function(figcaptionArg_, figcaptionArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "figcaption"
        }), _List_fromArray([figcaptionArg_, figcaptionArg_0]));
      }),
      dC: F2(function(figureArg_, figureArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "figure"
        }), _List_fromArray([figureArg_, figureArg_0]));
      }),
      dO: F2(function(footerArg_, footerArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "footer"
        }), _List_fromArray([footerArg_, footerArg_0]));
      }),
      dR: F2(function(formArg_, formArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "form"
        }), _List_fromArray([formArg_, formArg_0]));
      }),
      d3: F2(function(h1Arg_, h1Arg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "h1"
        }), _List_fromArray([h1Arg_, h1Arg_0]));
      }),
      d4: F2(function(h2Arg_, h2Arg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "h2"
        }), _List_fromArray([h2Arg_, h2Arg_0]));
      }),
      d5: F2(function(h3Arg_, h3Arg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "h3"
        }), _List_fromArray([h3Arg_, h3Arg_0]));
      }),
      d6: F2(function(h4Arg_, h4Arg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "h4"
        }), _List_fromArray([h4Arg_, h4Arg_0]));
      }),
      d7: F2(function(h5Arg_, h5Arg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "h5"
        }), _List_fromArray([h5Arg_, h5Arg_0]));
      }),
      d8: F2(function(h6Arg_, h6Arg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "h6"
        }), _List_fromArray([h6Arg_, h6Arg_0]));
      }),
      d9: F2(function(headerArg_, headerArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "header"
        }), _List_fromArray([headerArg_, headerArg_0]));
      }),
      ef: F2(function(hrArg_, hrArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "hr"
        }), _List_fromArray([hrArg_, hrArg_0]));
      }),
      ej: F2(function(iArg_, iArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "i"
        }), _List_fromArray([iArg_, iArg_0]));
      }),
      em: F2(function(iframeArg_, iframeArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "iframe"
        }), _List_fromArray([iframeArg_, iframeArg_0]));
      }),
      en: F2(function(imgArg_, imgArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "img"
        }), _List_fromArray([imgArg_, imgArg_0]));
      }),
      E: F2(function(inputArg_, inputArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "input"
        }), _List_fromArray([inputArg_, inputArg_0]));
      }),
      er: F2(function(insArg_, insArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "ins"
        }), _List_fromArray([insArg_, insArg_0]));
      }),
      ey: F2(function(kbdArg_, kbdArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "kbd"
        }), _List_fromArray([kbdArg_, kbdArg_0]));
      }),
      eD: F2(function(labelArg_, labelArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "label"
        }), _List_fromArray([labelArg_, labelArg_0]));
      }),
      eH: F2(function(legendArg_, legendArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "legend"
        }), _List_fromArray([legendArg_, legendArg_0]));
      }),
      eK: F2(function(liArg_, liArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "li"
        }), _List_fromArray([liArg_, liArg_0]));
      }),
      eS: F2(function(main_Arg_, main_Arg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "main_"
        }), _List_fromArray([main_Arg_, main_Arg_0]));
      }),
      eV: F2(function(mapArg_, mapArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]), $mdgriffith$elm_codegen$Elm$Annotation$var("msg")),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "map"
        }), _List_fromArray([mapArg_, mapArg_0]));
      }),
      e2: F2(function(markArg_, markArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "mark"
        }), _List_fromArray([markArg_, markArg_0]));
      }),
      e3: F2(function(mathArg_, mathArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "math"
        }), _List_fromArray([mathArg_, mathArg_0]));
      }),
      fa: F2(function(menuArg_, menuArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "menu"
        }), _List_fromArray([menuArg_, menuArg_0]));
      }),
      fb: F2(function(menuitemArg_, menuitemArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "menuitem"
        }), _List_fromArray([menuitemArg_, menuitemArg_0]));
      }),
      fd: F2(function(meterArg_, meterArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "meter"
        }), _List_fromArray([meterArg_, meterArg_0]));
      }),
      fo: F2(function(navArg_, navArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "nav"
        }), _List_fromArray([navArg_, navArg_0]));
      }),
      fp: F3(function(nodeArg_, nodeArg_0, nodeArg_1) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "node"
        }), _List_fromArray([nodeArg_, nodeArg_0, nodeArg_1]));
      }),
      fw: F2(function(objectArg_, objectArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "object"
        }), _List_fromArray([objectArg_, objectArg_0]));
      }),
      fx: F2(function(olArg_, olArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "ol"
        }), _List_fromArray([olArg_, olArg_0]));
      }),
      fN: F2(function(optgroupArg_, optgroupArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "optgroup"
        }), _List_fromArray([optgroupArg_, optgroupArg_0]));
      }),
      fO: F2(function(optionArg_, optionArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "option"
        }), _List_fromArray([optionArg_, optionArg_0]));
      }),
      fS: F2(function(outputArg_, outputArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "output"
        }), _List_fromArray([outputArg_, outputArg_0]));
      }),
      fT: F2(function(pArg_, pArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "p"
        }), _List_fromArray([pArg_, pArg_0]));
      }),
      fX: F2(function(paramArg_, paramArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "param"
        }), _List_fromArray([paramArg_, paramArg_0]));
      }),
      ga: F2(function(preArg_, preArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "pre"
        }), _List_fromArray([preArg_, preArg_0]));
      }),
      gf: F2(function(progressArg_, progressArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "progress"
        }), _List_fromArray([progressArg_, progressArg_0]));
      }),
      gj: F2(function(qArg_, qArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "q"
        }), _List_fromArray([qArg_, qArg_0]));
      }),
      gz: F2(function(rpArg_, rpArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "rp"
        }), _List_fromArray([rpArg_, rpArg_0]));
      }),
      gA: F2(function(rtArg_, rtArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "rt"
        }), _List_fromArray([rtArg_, rtArg_0]));
      }),
      gB: F2(function(rubyArg_, rubyArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "ruby"
        }), _List_fromArray([rubyArg_, rubyArg_0]));
      }),
      gD: F2(function(sArg_, sArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "s"
        }), _List_fromArray([sArg_, sArg_0]));
      }),
      gE: F2(function(sampArg_, sampArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "samp"
        }), _List_fromArray([sampArg_, sampArg_0]));
      }),
      gI: F2(function(sectionArg_, sectionArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "section"
        }), _List_fromArray([sectionArg_, sectionArg_0]));
      }),
      gJ: F2(function(selectArg_, selectArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "select"
        }), _List_fromArray([selectArg_, selectArg_0]));
      }),
      gU: F2(function(smallArg_, smallArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "small"
        }), _List_fromArray([smallArg_, smallArg_0]));
      }),
      gV: F2(function(sourceArg_, sourceArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "source"
        }), _List_fromArray([sourceArg_, sourceArg_0]));
      }),
      gX: F2(function(spanArg_, spanArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "span"
        }), _List_fromArray([spanArg_, spanArg_0]));
      }),
      g3: F2(function(strongArg_, strongArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "strong"
        }), _List_fromArray([strongArg_, strongArg_0]));
      }),
      g5: F2(function(subArg_, subArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "sub"
        }), _List_fromArray([subArg_, subArg_0]));
      }),
      g8: F2(function(summaryArg_, summaryArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "summary"
        }), _List_fromArray([summaryArg_, summaryArg_0]));
      }),
      g9: F2(function(supArg_, supArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "sup"
        }), _List_fromArray([supArg_, supArg_0]));
      }),
      hc: F2(function(tableArg_, tableArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "table"
        }), _List_fromArray([tableArg_, tableArg_0]));
      }),
      hg: F2(function(tbodyArg_, tbodyArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "tbody"
        }), _List_fromArray([tbodyArg_, tbodyArg_0]));
      }),
      hh: F2(function(tdArg_, tdArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "td"
        }), _List_fromArray([tdArg_, tdArg_0]));
      }),
      jy: function(textArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "text"
        }), _List_fromArray([textArg_]));
      },
      hl: F2(function(textareaArg_, textareaArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "textarea"
        }), _List_fromArray([textareaArg_, textareaArg_0]));
      }),
      hm: F2(function(tfootArg_, tfootArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "tfoot"
        }), _List_fromArray([tfootArg_, tfootArg_0]));
      }),
      hn: F2(function(thArg_, thArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "th"
        }), _List_fromArray([thArg_, thArg_0]));
      }),
      ho: F2(function(theadArg_, theadArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "thead"
        }), _List_fromArray([theadArg_, theadArg_0]));
      }),
      hp: F2(function(timeArg_, timeArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "time"
        }), _List_fromArray([timeArg_, timeArg_0]));
      }),
      hx: F2(function(trArg_, trArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "tr"
        }), _List_fromArray([trArg_, trArg_0]));
      }),
      hy: F2(function(trackArg_, trackArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "track"
        }), _List_fromArray([trackArg_, trackArg_0]));
      }),
      hI: F2(function(uArg_, uArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "u"
        }), _List_fromArray([uArg_, uArg_0]));
      }),
      hJ: F2(function(ulArg_, ulArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "ul"
        }), _List_fromArray([ulArg_, ulArg_0]));
      }),
      hV: F2(function(varArg_, varArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "var"
        }), _List_fromArray([varArg_, varArg_0]));
      }),
      h_: F2(function(videoArg_, videoArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "video"
        }), _List_fromArray([videoArg_, videoArg_0]));
      }),
      h1: F2(function(wbrArg_, wbrArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ]))),
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))),
          eo: _List_fromArray(["Html"]),
          ag: "wbr"
        }), _List_fromArray([wbrArg_, wbrArg_0]));
      })
    };
    var $author$project$Gen$Html$div = F2(function(divArg_, divArg_0) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
          $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ]))),
          $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
          ])))
        ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Html", _List_fromArray([
          $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
        ])))),
        eo: _List_fromArray(["Html"]),
        ag: "div"
      }), _List_fromArray([
        $mdgriffith$elm_codegen$Elm$list(divArg_),
        $mdgriffith$elm_codegen$Elm$list(divArg_0)
      ]));
    });
    var $author$project$Gen$Html$Attributes$style = F2(function(styleArg_, styleArg_0) {
      return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
        bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Html"]), "Attribute", _List_fromArray([
          $mdgriffith$elm_codegen$Elm$Annotation$var("msg")
        ])))),
        eo: _List_fromArray(["Html", "Attributes"]),
        ag: "style"
      }), _List_fromArray([
        $mdgriffith$elm_codegen$Elm$string(styleArg_),
        $mdgriffith$elm_codegen$Elm$string(styleArg_0)
      ]));
    });
    var $author$project$Ui$column = F2(function(attrs, children) {
      return A2($author$project$Gen$Html$div, A2($elm$core$List$append, _List_fromArray([
        A2($author$project$Gen$Html$Attributes$style, "display", "flex"),
        A2($author$project$Gen$Html$Attributes$style, "flex-direction", "column")
      ]), attrs), children);
    });
    var $author$project$Ui$el = F2(function(attrs, child) {
      return A2($author$project$Gen$Html$div, attrs, _List_fromArray([child]));
    });
    var $author$project$Ui$fill = "100%";
    var $author$project$Example$Interactive$getRunner = F2(function(runners, tipe) {
      return A3($elm$core$List$foldl, F2(function(run, maybe) {
        if (maybe.$ === 1) {
          return run.aN(tipe) ? $elm$core$Maybe$Just(run) : $elm$core$Maybe$Nothing;
        } else {
          return maybe;
        }
      }), $elm$core$Maybe$Nothing, runners);
    });
    var $author$project$Ui$height = function(value) {
      return A2($author$project$Gen$Html$Attributes$style, "height", value);
    };
    var $author$project$Ui$width = function(value) {
      return A2($author$project$Gen$Html$Attributes$style, "width", value);
    };
    var $author$project$Example$Interactive$build = F2(function(modul, targeting) {
      var _v0 = A2($author$project$Example$Interactive$buildExampleCallStack, modul, targeting);
      if (!_v0.$) {
        var example = _v0.a;
        var _v1 = A2($author$project$Example$Interactive$getRunner, targeting.gC, $author$project$Example$Type$getResultType(example.gt));
        if (_v1.$ === 1) {
          return $elm$core$Result$Err("No Runner!  Huh, this shouldn't happen");
        } else {
          var runner = _v1.a;
          var fields = _Utils_ap(runner.aU, example.bc.iu.js);
          return $elm$core$Result$Ok({
            aU: fields,
            ag: targeting.z.ag,
            bu: function(opts) {
              return A2($author$project$Ui$column, _List_fromArray([
                $author$project$Ui$width($author$project$Ui$fill),
                $author$project$Ui$height($author$project$Ui$fill),
                A2($author$project$Gen$Html$Attributes$style, "align-items", "center"),
                A2($author$project$Gen$Html$Attributes$style, "justify-content", "center")
              ]), _List_fromArray([
                A3($mdgriffith$elm_codegen$Elm$ifThen, opts.io, A2(runner.bu, opts, example.bc.c0), A2($author$project$Ui$el, _List_Nil, $author$project$Gen$Html$call_.jy(example.df.c0)))
              ]));
            }
          });
        }
      } else {
        var err = _v0.a;
        return $elm$core$Result$Err(err);
      }
    });
    var $author$project$Example$Type$primitiveNames = _List_fromArray(["String.String", "Basics.Bool", "Basics.Int", "Basics.Float"]);
    var $author$project$Example$Type$primitiveSingleContainers = _List_fromArray(["List.List", "Maybe.Maybe"]);
    var $author$project$Example$Type$isPrimitive = function(tipe) {
      isPrimitive:
        while (true) {
          switch (tipe.$) {
            case 0:
              if (tipe.a === "msg") {
                return true;
              } else {
                return false;
              }
            case 1:
              var arg = tipe.a;
              var result = tipe.b;
              return false;
            case 2:
              var tups = tipe.a;
              return A2($elm$core$List$all, $author$project$Example$Type$isPrimitive, tups);
            case 3:
              if (!tipe.b.b) {
                var name = tipe.a;
                return A2($elm$core$List$member, name, $author$project$Example$Type$primitiveNames);
              } else {
                if (!tipe.b.b.b) {
                  var name = tipe.a;
                  var _v1 = tipe.b;
                  var inner = _v1.a;
                  if (A2($elm$core$List$member, name, $author$project$Example$Type$primitiveSingleContainers)) {
                    var $temp$tipe = inner;
                    tipe = $temp$tipe;
                    continue isPrimitive;
                  } else {
                    return false;
                  }
                } else {
                  return false;
                }
              }
            default:
              var fields = tipe.a;
              var maybeName = tipe.b;
              return A2($elm$core$List$all, A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $author$project$Example$Type$isPrimitive), fields);
          }
        }
    };
    var $author$project$Example$Type$typeToStringHelper = F2(function(indent, tipe) {
      var indentStr = A2($elm$core$String$repeat, indent * 2, " ");
      switch (tipe.$) {
        case 0:
          var name = tipe.a;
          return name;
        case 1:
          var arg = tipe.a;
          var result = tipe.b;
          var resultStr = A2($author$project$Example$Type$typeToStringHelper, indent, result);
          var argStr = A2($author$project$Example$Type$typeToStringHelper, indent, arg);
          return "(" + (argStr + (`
` + (indentStr + ("-> " + (resultStr + ")")))));
        case 2:
          var types = tipe.a;
          if (!types.b) {
            return "()";
          } else {
            var typeStrs = A2($elm$core$List$map, $author$project$Example$Type$typeToStringHelper(indent), types);
            return "(" + (A2($elm$core$String$join, ", ", typeStrs) + ")");
          }
        case 3:
          var name = tipe.a;
          var typeArgs = tipe.b;
          if (!typeArgs.b) {
            return name;
          } else {
            var argStrs = A2($elm$core$List$map, $author$project$Example$Type$typeToStringHelper(indent), typeArgs);
            return name + (" " + A2($elm$core$String$join, " ", argStrs));
          }
        default:
          var fields = tipe.a;
          var maybeExtensible = tipe.b;
          var fieldStrs = A2($elm$core$List$map, function(_v5) {
            var fieldName = _v5.a;
            var fieldType = _v5.b;
            return indentStr + (fieldName + (" : " + A2($author$project$Example$Type$typeToStringHelper, indent + 1, fieldType)));
          }, fields);
          var fieldsStr = A2($elm$core$String$join, `
`, fieldStrs);
          var extensiblePrefix = function() {
            if (!maybeExtensible.$) {
              var extName = maybeExtensible.a;
              return extName + " | ";
            } else {
              return "";
            }
          }();
          if (!fields.b) {
            return "{}";
          } else {
            return `{
` + (extensiblePrefix + (fieldsStr + (`
` + (A2($elm$core$String$repeat, (indent - 1) * 2, " ") + "}"))));
          }
      }
    });
    var $author$project$Example$Type$typeToString = function(tipe) {
      return A2($author$project$Example$Type$typeToStringHelper, 0, tipe);
    };
    var $author$project$Example$Type$debugIsStartingPointHelper = F2(function(tipe, pieces) {
      debugIsStartingPointHelper:
        while (true) {
          if (tipe.$ === 1) {
            var arg = tipe.a;
            var result = tipe.b;
            if ($author$project$Example$Type$isPrimitive(arg)) {
              var $temp$tipe = result, $temp$pieces = A2($elm$core$List$cons, "yes, " + $author$project$Example$Type$typeToString(arg), pieces);
              tipe = $temp$tipe;
              pieces = $temp$pieces;
              continue debugIsStartingPointHelper;
            } else {
              return A2($elm$core$List$cons, "Not a primitive, skipping " + $author$project$Example$Type$typeToString(arg), pieces);
            }
          } else {
            return A2($elm$core$List$cons, "Dont need to see anymore, it is", pieces);
          }
        }
    });
    var $author$project$Example$Type$isBuilder = function(possibleBuilder) {
      isBuilder:
        while (true) {
          if (possibleBuilder.$ === 1) {
            var arg = possibleBuilder.a;
            var result = possibleBuilder.b;
            if (A2($author$project$Example$Type$matches, result, arg)) {
              return true;
            } else {
              var $temp$possibleBuilder = result;
              possibleBuilder = $temp$possibleBuilder;
              continue isBuilder;
            }
          } else {
            return false;
          }
        }
    };
    var $author$project$Example$Type$debugStartingPoint = function(tipe) {
      return $author$project$Example$Type$isBuilder(tipe) ? _List_fromArray(["Is Builder, skipping"]) : $elm$core$List$reverse(A2($author$project$Example$Type$debugIsStartingPointHelper, tipe, _List_Nil));
    };
    var $author$project$Example$Type$isStartingPointHelper = function(tipe) {
      isStartingPointHelper:
        while (true) {
          if (tipe.$ === 1) {
            var arg = tipe.a;
            var result = tipe.b;
            if ($author$project$Example$Type$isPrimitive(arg)) {
              var $temp$tipe = result;
              tipe = $temp$tipe;
              continue isStartingPointHelper;
            } else {
              return false;
            }
          } else {
            return true;
          }
        }
    };
    var $author$project$Example$Type$isStartingPoint = function(tipe) {
      return $author$project$Example$Type$isBuilder(tipe) ? false : $author$project$Example$Type$isStartingPointHelper(tipe);
    };
    var $author$project$Exemplar$html = {
      aN: function(tipe) {
        if (tipe.$ === 3 && tipe.a === "Html.Html") {
          return true;
        } else {
          return false;
        }
      },
      aU: _List_Nil,
      bu: F2(function(_v1, val) {
        var model = _v1.av;
        var onChange = _v1.a6;
        return val;
      })
    };
    var $author$project$Gen$Parser$call_ = {
      bL: F2(function(andThenArg_, andThenArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("b")
            ]))),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("b")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "andThen"
        }), _List_fromArray([andThenArg_, andThenArg_0]));
      }),
      b_: function(backtrackableArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "backtrackable"
        }), _List_fromArray([backtrackableArg_]));
      },
      cd: function(chompIfArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$char]), $mdgriffith$elm_codegen$Elm$Annotation$bool)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
          eo: _List_fromArray(["Parser"]),
          ag: "chompIf"
        }), _List_fromArray([chompIfArg_]));
      },
      ce: function(chompUntilArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
          eo: _List_fromArray(["Parser"]),
          ag: "chompUntil"
        }), _List_fromArray([chompUntilArg_]));
      },
      cf: function(chompUntilEndOrArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
          eo: _List_fromArray(["Parser"]),
          ag: "chompUntilEndOr"
        }), _List_fromArray([chompUntilEndOrArg_]));
      },
      cg: function(chompWhileArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$char]), $mdgriffith$elm_codegen$Elm$Annotation$bool)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
          eo: _List_fromArray(["Parser"]),
          ag: "chompWhile"
        }), _List_fromArray([chompWhileArg_]));
      },
      cq: function(commitArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "commit"
        }), _List_fromArray([commitArg_]));
      },
      cH: function(deadEndsToStringArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "DeadEnd", _List_Nil))
          ]), $mdgriffith$elm_codegen$Elm$Annotation$string)),
          eo: _List_fromArray(["Parser"]),
          ag: "deadEndsToString"
        }), _List_fromArray([deadEndsToStringArg_]));
      },
      d$: function(getChompedStringArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string])))),
          eo: _List_fromArray(["Parser"]),
          ag: "getChompedString"
        }), _List_fromArray([getChompedStringArg_]));
      },
      eB: function(keywordArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
          eo: _List_fromArray(["Parser"]),
          ag: "keyword"
        }), _List_fromArray([keywordArg_]));
      },
      eF: function(lazyArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$unit]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "lazy"
        }), _List_fromArray([lazyArg_]));
      },
      eM: function(lineCommentArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
          eo: _List_fromArray(["Parser"]),
          ag: "lineComment"
        }), _List_fromArray([lineCommentArg_]));
      },
      eP: F2(function(loopArg_, loopArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("state"),
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("state")
            ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
              A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Step", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("state"),
                $mdgriffith$elm_codegen$Elm$Annotation$var("a")
              ]))
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "loop"
        }), _List_fromArray([loopArg_, loopArg_0]));
      }),
      eV: F2(function(mapArg_, mapArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]), $mdgriffith$elm_codegen$Elm$Annotation$var("b")),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("b")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "map"
        }), _List_fromArray([mapArg_, mapArg_0]));
      }),
      e1: F2(function(mapChompedStringArg_, mapChompedStringArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$string,
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]), $mdgriffith$elm_codegen$Elm$Annotation$var("b")),
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("b")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "mapChompedString"
        }), _List_fromArray([mapChompedStringArg_, mapChompedStringArg_0]));
      }),
      fk: F3(function(multiCommentArg_, multiCommentArg_0, multiCommentArg_1) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            $mdgriffith$elm_codegen$Elm$Annotation$string,
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Nestable", _List_Nil)
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
          eo: _List_fromArray(["Parser"]),
          ag: "multiComment"
        }), _List_fromArray([multiCommentArg_, multiCommentArg_0, multiCommentArg_1]));
      }),
      fv: function(numberArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("int", $mdgriffith$elm_codegen$Elm$Annotation$maybe(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$int]), $mdgriffith$elm_codegen$Elm$Annotation$var("a")))),
              _Utils_Tuple2("hex", $mdgriffith$elm_codegen$Elm$Annotation$maybe(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$int]), $mdgriffith$elm_codegen$Elm$Annotation$var("a")))),
              _Utils_Tuple2("octal", $mdgriffith$elm_codegen$Elm$Annotation$maybe(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$int]), $mdgriffith$elm_codegen$Elm$Annotation$var("a")))),
              _Utils_Tuple2("binary", $mdgriffith$elm_codegen$Elm$Annotation$maybe(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$int]), $mdgriffith$elm_codegen$Elm$Annotation$var("a")))),
              _Utils_Tuple2("float", $mdgriffith$elm_codegen$Elm$Annotation$maybe(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$float]), $mdgriffith$elm_codegen$Elm$Annotation$var("a"))))
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "number"
        }), _List_fromArray([numberArg_]));
      },
      az: function(oneOfArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ])))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "oneOf"
        }), _List_fromArray([oneOfArg_]));
      },
      U: function(problemArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "problem"
        }), _List_fromArray([problemArg_]));
      },
      jo: F2(function(runArg_, runArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ])),
            $mdgriffith$elm_codegen$Elm$Annotation$string
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Result"]), "Result", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list(A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "DeadEnd", _List_Nil)),
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "run"
        }), _List_fromArray([runArg_, runArg_0]));
      }),
      gN: function(sequenceArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("start", $mdgriffith$elm_codegen$Elm$Annotation$string),
              _Utils_Tuple2("separator", $mdgriffith$elm_codegen$Elm$Annotation$string),
              _Utils_Tuple2("end", $mdgriffith$elm_codegen$Elm$Annotation$string),
              _Utils_Tuple2("spaces", A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$unit]))),
              _Utils_Tuple2("item", A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
                $mdgriffith$elm_codegen$Elm$Annotation$var("a")
              ]))),
              _Utils_Tuple2("trailing", A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Trailing", _List_Nil))
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$var("a"))
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "sequence"
        }), _List_fromArray([sequenceArg_]));
      },
      g7: function(succeedArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "succeed"
        }), _List_fromArray([succeedArg_]));
      },
      ha: function(symbolArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
          eo: _List_fromArray(["Parser"]),
          ag: "symbol"
        }), _List_fromArray([symbolArg_]));
      },
      hw: function(tokenArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
          eo: _List_fromArray(["Parser"]),
          ag: "token"
        }), _List_fromArray([tokenArg_]));
      },
      hW: function(variableArg_) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$record(_List_fromArray([
              _Utils_Tuple2("start", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$char]), $mdgriffith$elm_codegen$Elm$Annotation$bool)),
              _Utils_Tuple2("inner", A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$char]), $mdgriffith$elm_codegen$Elm$Annotation$bool)),
              _Utils_Tuple2("reserved", A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Set"]), "Set", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string])))
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([$mdgriffith$elm_codegen$Elm$Annotation$string])))),
          eo: _List_fromArray(["Parser"]),
          ag: "variable"
        }), _List_fromArray([variableArg_]));
      },
      h5: F2(function(withIndentArg_, withIndentArg_0) {
        return A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
          bM: $elm$core$Maybe$Just(A2($mdgriffith$elm_codegen$Elm$Annotation$function, _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$int,
            A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
              $mdgriffith$elm_codegen$Elm$Annotation$var("a")
            ]))
          ]), A3($mdgriffith$elm_codegen$Elm$Annotation$namedWith, _List_fromArray(["Parser"]), "Parser", _List_fromArray([
            $mdgriffith$elm_codegen$Elm$Annotation$var("a")
          ])))),
          eo: _List_fromArray(["Parser"]),
          ag: "withIndent"
        }), _List_fromArray([withIndentArg_, withIndentArg_0]));
      })
    };
    var $author$project$Ui$htmlAttribute = function(attr) {
      return attr;
    };
    var $author$project$Ui$paddingXY = F2(function(x, y) {
      return A2($author$project$Gen$Html$Attributes$style, "padding", $elm$core$String$fromInt(x) + ("px " + ($elm$core$String$fromInt(y) + "px")));
    });
    var $author$project$Exemplar$parser = {
      aN: function(tipe) {
        if (tipe.$ === 3 && tipe.a === "Parser.Parser") {
          return true;
        } else {
          return false;
        }
      },
      aU: _List_fromArray([
        A2($author$project$Interactive$field, "Source", {
          J: $mdgriffith$elm_codegen$Elm$string("# Hello"),
          E: $author$project$Interactive$string
        })
      ]),
      bu: F2(function(_v1, foundParser) {
        var model = _v1.av;
        var onChange = _v1.a6;
        return A2($mdgriffith$elm_codegen$Elm$Case$result, A2($author$project$Gen$Parser$call_.jo, foundParser, A2($mdgriffith$elm_codegen$Elm$get, "source", model)), {
          iD: A2($elm$core$Tuple$pair, "err", function(err) {
            return A2($author$project$Ui$el, _List_fromArray([
              A2($author$project$Ui$paddingXY, 32, 0),
              $author$project$Ui$width($author$project$Ui$fill),
              $author$project$Ui$htmlAttribute(A2($author$project$Gen$Html$Attributes$style, "background", "rgb(36,36,36)"))
            ]), $author$project$Gen$Html$call_.jy(A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
              bM: $elm$core$Maybe$Nothing,
              eo: _List_fromArray(["Debug"]),
              ag: "toString"
            }), _List_fromArray([err]))));
          }),
          i7: A2($elm$core$Tuple$pair, "ok", function(ok) {
            return A2($author$project$Ui$el, _List_fromArray([
              A2($author$project$Ui$paddingXY, 32, 0),
              $author$project$Ui$width($author$project$Ui$fill),
              $author$project$Ui$htmlAttribute(A2($author$project$Gen$Html$Attributes$style, "background", "rgb(36,36,36)"))
            ]), $author$project$Gen$Html$call_.jy(A2($mdgriffith$elm_codegen$Elm$apply, $mdgriffith$elm_codegen$Elm$value({
              bM: $elm$core$Maybe$Nothing,
              eo: _List_fromArray(["Debug"]),
              ag: "toString"
            }), _List_fromArray([ok]))));
          })
        });
      })
    };
    var $author$project$Exemplar$string = {
      aN: function(tipe) {
        if (tipe.$ === 3 && tipe.a === "String.String" && !tipe.b.b) {
          return true;
        } else {
          return false;
        }
      },
      aU: _List_Nil,
      bu: F2(function(_v1, val) {
        var model = _v1.av;
        var onChange = _v1.a6;
        return $author$project$Gen$Html$call_.jy(val);
      })
    };
    var $author$project$Exemplar$runners = _List_fromArray([$author$project$Exemplar$parser, $author$project$Exemplar$html, $author$project$Exemplar$string]);
    var $author$project$Exemplar$interactiveAll = function(mod) {
      return {
        dg: A3($elm$core$List$foldl, F2(function(val, examples) {
          if ($author$project$Example$Type$isStartingPoint(val.ak)) {
            var builtResult = A2($author$project$Example$Interactive$build, mod, { gC: $author$project$Exemplar$runners, z: val });
            if (builtResult.$ === 1) {
              var err = builtResult.a;
              return A2($elm$core$List$cons, $elm$core$Result$Err({ cP: err, ag: val.ag }), examples);
            } else {
              var examp = builtResult.a;
              return A2($elm$core$List$cons, $elm$core$Result$Ok(examp), examples);
            }
          } else {
            var isBuilder = $author$project$Example$Type$isBuilder(val.ak) ? "true" : "false";
            var debugStartingPoint = A2($elm$core$String$join, `
`, $author$project$Example$Type$debugStartingPoint(val.ak));
            return A2($elm$core$List$cons, $elm$core$Result$Err({ cP: "Not a starting point `" + (val.ag + ("`" + (`
` + ("isBuilder: " + isBuilder))) + (`
` + debugStartingPoint)), ag: val.ag }), examples);
          }
        }), _List_Nil, mod.jC),
        ag: mod.ag
      };
    };
    var $author$project$Generate$renderExampleModule = function(mod) {
      var interactiveMod = $author$project$Exemplar$interactiveAll(mod);
      var moduleName = A2($elm$core$List$cons, "Dev", A2($elm$core$String$split, ".", interactiveMod.ag));
      return A2($elm$core$List$map, function(exampleResult) {
        if (!exampleResult.$) {
          var example = exampleResult.a;
          return A2($author$project$Interactive$generateSingle, _Utils_ap(moduleName, _List_fromArray([
            $author$project$Extra$String$capitalize(example.ag)
          ])), example);
        } else {
          var err = exampleResult.a;
          return A2($mdgriffith$elm_codegen$Elm$file, _Utils_ap(moduleName, _List_fromArray([
            $author$project$Extra$String$capitalize(err.ag)
          ])), _List_fromArray([
            A2($mdgriffith$elm_codegen$Elm$declaration, "error", $mdgriffith$elm_codegen$Elm$string(err.cP))
          ]));
        }
      }, interactiveMod.dg);
    };
    var $elm$json$Json$Decode$value = _Json_decodeValue;
    var $author$project$Generate$main = A2($author$project$Gen$CodeGen$Generate$fromJson, $author$project$Options$decoder, function(options) {
      return A2($elm$core$List$concatMap, $author$project$Generate$renderExampleModule, options.jh);
    });
    _Platform_export({ Generate: { init: $author$project$Generate$main($elm$json$Json$Decode$value)(0) } });
  })(exports);
});

// interactiveIndex.ts
var ElmGenerator = require_interactive_generate();
async function run(flags) {
  return new Promise((resolve, reject) => {
    const app = ElmGenerator.Elm.Generate.init({ flags });
    if (app.ports.onSuccessSend) {
      app.ports.onSuccessSend.subscribe(resolve);
    }
    if (app.ports.onInfoSend) {
      app.ports.onInfoSend.subscribe((info) => console.log(info));
    }
    if (app.ports.onFailureSend) {
      app.ports.onFailureSend.subscribe(reject);
    }
  }).then((files) => {
    const generated = [];
    for (const file of files) {
      generated.push({
        outputDir: "virtual",
        path: file.path,
        contents: file.contents
      });
    }
    return { generated };
  }).catch((errorList) => {
    const errors = [];
    for (const error of errorList) {
      errors.push({
        title: error.title,
        description: error.description
      });
    }
    return { errors };
  });
}
var data = "";
process.stdin.setEncoding("utf-8");
process.stdin.on("data", (chunk) => {
  data += chunk;
});
process.stdin.on("end", async () => {
  try {
    const input = JSON.parse(data);
    if (!input.flags) {
      console.error('Input must contain "flags" properties');
      process.exit(1);
    }
    const summary = await run(input.flags).catch((err) => {
      console.error(err);
      process.exit(1);
    });
    console.log(JSON.stringify(summary));
  } catch (err) {
    console.error("Failed to parse JSON input:", err);
    process.exit(1);
  }
});
export {
  run
};
