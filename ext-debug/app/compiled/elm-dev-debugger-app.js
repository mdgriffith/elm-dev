(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
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

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
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

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
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

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
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

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
			// Here’s how `task.c` comes to existence:
			//
			// _Scheduler_binding(function (callback) {
			//     // Do stuff and use callback.
			//     // The below function is assigned to `task.c` by `_Scheduler_step`.
			//     return function() {
			//         // Do cleanup. Doesn’t use callback, but technically has it in scope.
			//     }
			// });
			//
			// Since `task.c` has that `callback` in scope, which eventually references
			// `sendToApp`, which has access to `model` etc., it’s important to remove it
			// after using it. Otherwise it can prevent a stopped app from being garbage collected.
			// `null` is the default value for `c` as seen in `_Scheduler_binding`.
			// It also doesn’t make sense to kill the same task twice anyway.
			task.c = null;
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F3(function(impl, flagDecoder, debugMetadata)
{
	var init = function(args)
	{
		return _Platform_initialize(
			flagDecoder,
			args,
			null,
			null,
			null,
			function() { return function() {} },
			impl
		);
	};

	/**/
	init.hotReloadData = {
		impl: impl,
		platform_effectManagers: _Platform_effectManagers,
		scheduler_enqueue: _Scheduler_enqueue
	};
	//*/

	return init;
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, _init, _update, _subscriptions, stepperBuilder, impl)
{
	// Old versions of elm/browser do not send the `impl` – instead, they send parts of it separately.
	// Sending the whole object is required for hot reloading.
	if (!impl)
	{
		impl = {
			init: _init,
			update: _update,
			subscriptions: _subscriptions
		}
	}

	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = impl.init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model, true);
	var ports = _Platform_setupEffects(managers, sendToApp);
	var stopped = false;

	function sendToApp(msg, viewMetadata)
	{
		if (stopped)
		{
			return;
		}
		var pair = A2(impl.update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, impl.subscriptions(model));
	}

	// Initial draw. This used to be done as a side effect of calling `stepperBuilder`.
	// `stepper.shutdown` was introduced at the same time as moving the draw call here,
	// so we can use that to know if we should draw or not. Drawing here has two benefits:
	// Firstly, it’s easier to understand. No hidden side effects. The effects are concentrated here.
	// Secondly, drawing can have side effects, because of custom elements. Their `connectedCallback`s
	// fire while drawing, and can dispatch (synchronous) events which cause an update. That won’t
	// work if the draw happened during `stepperBuilder`, since `stepper` won’t be defined here yet.
	if (stepper.shutdown)
	{
		stepper(model, true);
	}

	// Apply `Cmd`s from `init`, and run `subscriptions` for the first time.
	_Platform_enqueueEffects(managers, initPair.b, impl.subscriptions(model));

	var detachedDomNode = undefined;

	var detachView = function()
	{
		// Make a second call to `app.detachView` do nothing.
		if (detachedDomNode !== undefined)
		{
			return detachedDomNode;
		}

		// Draw synchronously one last time in case we’re waiting for an animation frame.
		stepper(model, true);

		var stepperShutdown = stepper.shutdown;

		// Prevent new draw calls from being made.
		stepper = function() {};

		// Remove event listeners from the DOM.
		// This is only available in newer versions of elm/browser.
		// Also note that `_Platform_worker` does not provide it (since there’s no `view`).
		detachedDomNode = stepperShutdown ? stepperShutdown() : null;

		// Return the final DOM node produced by Elm (if any).
		// This is necessarily not the same DOM node as the app was mounted on,
		// since Elm might replace it, for example if the element type changes.
		return detachedDomNode;
	};

	var stop = function()
	{
		// Make a second call to `app.stop` do nothing.
		if (stopped)
		{
			return detachedDomNode;
		}

		// Stop view. This can trigger synchronous updates, which is why we haven’t stopped updates yet.
		detachView();

		// Stop update. (This makes us ignore messages.)
		stopped = true;

		// Stop subscriptions.
		_Platform_enqueueEffects(managers, _Platform_batch(_List_Nil), _Platform_batch(_List_Nil));

		// Same return value as `app.detachView`.
		return detachedDomNode;
	};

	var app = {
		detachView: detachView,
		stop: stop
	};

	if (ports)
	{
		app.ports = ports;
	}

	/**/
	app.hotReload = function(hotReloadData)
	{
		// Remove old subscriptions.
		_Platform_enqueueEffects(managers, _Platform_batch(_List_Nil), _Platform_batch(_List_Nil));

		// This function depends on local state, so we need to update it to the implementation from the new code.
		_Scheduler_enqueue = hotReloadData.scheduler_enqueue;

		// Setup any new effect managers.
		var newPorts = _Platform_hotReloadEffects(managers, ports, hotReloadData.platform_effectManagers, sendToApp);
		if (!ports && newPorts)
		{
			app.ports = newPorts;
		}
		ports = newPorts;

		// Replace view, update and subscriptions with implementations from the new code.
		for (var key in hotReloadData.impl)
		{
			impl[key] = hotReloadData.impl[key];
		}

		// Draw synchronously with the new view function.
		stepper(model, true);

		// Set up new subscriptions.
		_Platform_enqueueEffects(managers, _Platform_batch(_List_Nil), impl.subscriptions(model));
	};
	//*/

	return app;
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			var data = manager.a(key, sendToApp);
			ports[key] = data.b;
			managers[key] = _Platform_instantiateManager(
				data.c,
				data.d,
				null,
				manager.e,
				manager.f,
				sendToApp
			);
		}
		else
		{
			managers[key] = _Platform_instantiateManager(
				manager.c,
				manager.d,
				manager.g,
				manager.e,
				manager.f,
				sendToApp
			);
		}
	}

	return ports;
}


function _Platform_hotReloadEffects(managers, ports, newEffectManagers, sendToApp)
{
	for (var key in newEffectManagers)
	{
		var manager = newEffectManagers[key];
		var existing = _Platform_effectManagers[key];

		if (manager.a)
		{
			// If a port already exists and is still outgoing or still incoming:
			if (existing && (manager.e && existing.e || manager.f && existing.f))
			{
				// Update its converter in case it has been changed to let different data through.
				existing.h = manager.h;
			}
			else
			{
				// Otherwise instantiate a new port. JavaScript already subscribed to the old port
				// won’t get any more data there (since the new Elm code never sends any).
				// JavaScript trying to send data through the old port won’t achieve anything,
				// since the new Elm code doesn’t have it in its subscriptions.
				_Platform_effectManagers[key] = manager;
				ports = ports || {};
				var data = manager.a(key, sendToApp);
				ports[key] = data.b;
				managers[key] = _Platform_instantiateManager(
					data.c,
					data.d,
					null,
					manager.e,
					manager.f,
					sendToApp
				);
			}
		}
		else if (!existing)
		{
			// Instantiate new, non-port effect managers.
			_Platform_effectManagers[key] = manager;
			managers[key] = _Platform_instantiateManager(
				manager.c,
				manager.d,
				manager.g,
				manager.e,
				manager.f,
				sendToApp
			);
		}
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		c: init,
		d: onEffects,
		g: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(init, onEffects, onSelfMsg, cmdMap, subMap, sendToApp)
{
	var router = {
		i: sendToApp,
		j: undefined
	};

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.k, value.l, state)
				: A3(onEffects, router, cmdMap ? value.k : value.l, state);
		}));
	}

	return router.j = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, init));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.i(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.j, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			m: home,
			n: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		o: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		p: tagger,
		q: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ r: managers, s: cmdBag, t: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.r, fx.s, fx.t);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { k: _List_Nil, l: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.m;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.n);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.o; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.q, effectsDict, {
				u: bag.p,
				v: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.v)
		{
			x = temp.u(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { k: _List_Nil, l: _List_Nil };

	isCmd
		? (effects.k = _List_Cons(newEffect, effects.k))
		: (effects.l = _List_Cons(newEffect, effects.l));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		h: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].h;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	var onEffects = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		c: init,
		d: onEffects,
		b: {
			subscribe: subscribe,
			unsubscribe: unsubscribe
		}
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		h: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].h;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	var onEffects = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return {
		c: init,
		d: onEffects,
		b: { send: send }
	};
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	if (!('Elm' in scope))
	{
		// Use `defineProperty` to make `.hot` non-enumerable, so that a for-in loop on `scope['Elm']`
		// only includes Elm modules. We use that below, and end user code might depend on it too.
		scope['Elm'] = Object.defineProperty({}, 'hot', {
			value: {
				// Usage example:
				//     const f = new Function(newCompiledElmCodeAsString);
				//     const newScope = {};
				//     f.call(newScope);
				//     Elm.hot.reload(newScope);
				reload: function(newScope)
				{
					// Update Elm modules.
					_Platform_mergeExportsHotReload(scope['Elm'], newScope['Elm']);

					// Update hot reload data.
					for (var key in newScope['Elm'].hot.w)
					{
						scope['Elm'].hot.w[key] = newScope['Elm'].hot.w[key];
					}

					// Make the _new_ code push to the _old_ list of reload functions,
					// and read hot reload data from the _old_ dict (which is always merged with the latest above).
					var reloadFunctions = newScope['Elm'].hot.x = scope['Elm'].hot.x;
					newScope['Elm'].hot.w = scope['Elm'].hot.w;

					// Reload all running Elm app instances.
					for (var i = 0; i < reloadFunctions.length; i++)
					{
						reloadFunctions[i]();
					}
				},
				x: [],
				w: {}
			}
		});
	}
	_Platform_mergeExportsDebug('Elm', scope['Elm'], exports);
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		var exp = exports[name];
		if (name == 'init')
		{
			if (name in obj)
			{
				_Debug_crash(6, moduleName)
			}
			else
			{
				obj[name] = _Platform_wrapInit(moduleName, exp);
				scope['Elm'].hot.w[moduleName] = exp.hotReloadData;
				delete exp.hotReloadData;
			}
		}
		else
		{
			_Platform_mergeExportsDebug(moduleName + '.' + name, obj[name] || (obj[name] = {}), exp);
		}
	}
}


function _Platform_mergeExportsHotReload(obj, exports)
{
	for (var name in exports)
	{
		(name in obj && name != 'init')
			? _Platform_mergeExportsHotReload(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_wrapInit(moduleName, init)
{
	return function(args)
	{
		var app = init(args);

		var hotReload = app.hotReload;
		delete app.hotReload;
		var reloadFunction = function ()
		{
			hotReload(scope['Elm'].hot.w[moduleName]);
		};
		scope['Elm'].hot.x.push(reloadFunction);

		var stop = app.stop;
		app.stop = function()
		{
			var index = scope['Elm'].hot.x.indexOf(reloadFunction);
			if (index !== -1)
			{
				scope['Elm'].hot.x.splice(index, 1);
			}
			return stop();
		};

		return app;
	};
}




// Double underscore sequences are replaced with single letters or numbers.
// Exactly which letter or number is used depends on the order the properties are first mentioned.
// This preserves the letters and numbers from v1.0.3 for compatibility with tools that assume those exact values.
// elm-explorations/test: https://github.com/elm-explorations/test/blob/d5eb84809de0f8bbf50303efd26889092c800609/src/Elm/Kernel/HtmlAsJson.js
// elm-pages: https://github.com/dillonkearns/elm-pages/blob/fa1d0347016e20917b412de5c3657c2e6e095087/generator/src/build.js#L642
// The list of names was extracted using the following commands:
// # Switch to the reference commit:
// git switch $old
// # Find all relevant double underscore tokens.
// grep --only --extended-regexp '_{2}[0-9a-z]\w+' src/Elm/Kernel/VirtualDom.js | awk '!visited[$0]++' >a.txt
// # Switch to the current commit:
// git switch $new
// # Exclude the below line, then find all relevant double underscore tokens.
// grep --invert-match void src/Elm/Kernel/VirtualDom.js | grep --only --extended-regexp '_{2}[0-9a-z]\w+' | awk '!visited[$0]++' >b.txt
// # Keep only the double underscore tokens from the reference commit that still exist.
// grep --fixed-strings --line-regexp --file=b.txt a.txt
void { 0: null, a: null, b: null, 1: null, c: null, d: null, e: null, f: null, 2: null, 3: null, g: null, h: null, i: null, 4: null, j: null, k: null, 5: null, l: null, m: null, 0: null, n: null, o: null, 1: null, 2: null, 3: null, 4: null, p: null, q: null };



// HELPERS


var _VirtualDom_everTranslated = false;

var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

function _VirtualDom_insertBefore(parent, child, reference)
{
	if (!(child.parentNode === parent && child.nextSibling === reference))
	{
		parent.insertBefore(child, reference);
	}
}

function _VirtualDom_insertAfter(parent, child, reference)
{
	if (!(child.parentNode === parent && child.previousSibling === reference))
	{
		parent.insertBefore(child, reference === null ? parent.firstChild : reference.nextSibling);
	}
}

function _VirtualDom_moveBefore_(parent, child, reference)
{
	if (!(child.parentNode === parent && child.nextSibling === reference))
	{
		parent.moveBefore(child, reference);
	}
}

function _VirtualDom_moveAfter_(parent, child, reference)
{
	if (!(child.parentNode === parent && child.previousSibling === reference))
	{
		parent.moveBefore(child, reference === null ? parent.firstChild : reference.nextSibling);
	}
}

var _VirtualDom_supports_moveBefore = typeof Element !== 'undefined' && typeof Element.prototype.moveBefore === 'function';

var _VirtualDom_moveBefore = _VirtualDom_supports_moveBefore ? _VirtualDom_moveBefore_ : _VirtualDom_insertBefore;

var _VirtualDom_moveAfter = _VirtualDom_supports_moveBefore ? _VirtualDom_moveAfter_ : _VirtualDom_insertAfter;

function _VirtualDom_remove(domNode)
{
	// An extension might have (re-)moved the element, so even if we have a
	// reference to the `parentDomNode` that _should_ be the parent, we can’t
	// just call `parentDomNode.removeChild(domNode)`. That throws an error if
	// the node is not a child of `parentDomNode`.
	var parentNode = domNode.parentNode;
	if (parentNode)
	{
		parentNode.removeChild(domNode);
	}
}

// A `tNode`, or “tree node”, is a tree structure that contains DOM nodes. The
// children are keyed by index for regular nodes, and by key for keyed nodes.
// This tree structure always matches the latest rendered virtual DOM tree,
// while the real DOM tree might have been modified by browser extensions, page
// translators and third party scripts. By using our own tree, we can guarantee
// access to the DOM nodes we need, even if someone else has changed the page.
function _VirtualDom_createTNode(domNode)
{
	return {
		r: domNode,
		s: Object.create(null)
	};
}

var _VirtualDom_init = F3(function(virtualNode, flagDecoder, debugMetadata)
{
	// NOTE: this function needs _Platform_export available to work

	var init = function(args)
	{
		/**_UNUSED/
		var node = args['node'];
		//*/
		/**/
		var node = args && args['node'] ? args['node'] : _Debug_crash(0);
		//*/

		var sendToApp = function() {};
		var tNode = _VirtualDom_createTNode(undefined);
		var nextNode = _VirtualDom_render(virtualNode, sendToApp, tNode);
		nextNode.elmTree = tNode;
		node.parentNode.replaceChild(nextNode, node);
		node = nextNode;

		var stopped = false;
		var detachView = function()
		{
			// Make a second call to `app.detachView` do nothing.
			if (stopped)
			{
				return node;
			}
			stopped = true;
			_VirtualDom_removeAllEventListeners(node);
			return node;
		};

		var app = {
			detachView: detachView,
			stop: detachView
		};

		/**/
		app.hotReload = function(hotReloadData)
		{
			node = _VirtualDom_applyPatches(node, virtualNode, hotReloadData.virtualNode, sendToApp);
			virtualNode = hotReloadData.virtualNode;
		};
		//*/

		return app;
	};

	/**/
	init.hotReloadData = {
		virtualNode: virtualNode
	};
	//*/

	return init;
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = []; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			kids.push(kidList.a);
		}

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			// Unused, only exists for backwards compatibility with:
			// https://github.com/elm-explorations/test/blob/9669a27d84fc29175364c7a60d5d700771a2801e/src/Test/Html/Internal/ElmHtml/InternalTypes.elm#L279
			// https://github.com/dillonkearns/elm-pages/blob/fa1d0347016e20917b412de5c3657c2e6e095087/src/Test/Html/Internal/ElmHtml/InternalTypes.elm#L281
			b: 0
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], kidsMap = Object.create(null); kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			var key = kid.a;
			// Handle duplicate keys by adding a postfix.
			while (key in kidsMap)
			{
				key += _VirtualDom_POSTFIX;
				kid = _Utils_Tuple2(key, kid.b);
			}
			kids.push(kid);
			kidsMap[key] = kid.b;
		}

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			// e holds the order and length of the kids.
			e: kids,
			// t is a dict from key to node.
			// Note when iterating JavaScript objects, numeric-looking keys come first.
			// So we need both e and t.
			// Another reason is backwards compatibility with:
			// https://github.com/elm-explorations/test/blob/d5eb84809de0f8bbf50303efd26889092c800609/src/Elm/Kernel/HtmlAsJson.js#L37
			// https://github.com/dillonkearns/elm-pages/blob/fa1d0347016e20917b412de5c3657c2e6e095087/generator/src/build.js#L675
			t: kidsMap,
			f: namespace,
			b: 0 // See _VirtualDom_nodeNS.
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 0 // See _VirtualDom_nodeNS.
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'outerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


// This boolean is used to turn the `class` attribute into the `className` property only when needed for
// backwards compatibility with elm-exploration/test (which only looks for `className` since `Html.Attributes.class` used to be implemented that way):
// https://github.com/elm-explorations/test/blob/eef7f1aad0cc8c8b1434c678c757a1429fbcb9c7/src/Test/Html/Internal/ElmHtml/Query.elm#L265
// Why not just keep `Html.Attributes.class` implemented as `className` then? Well, `Html.Attributes.class`
// is better implemented as a `class` attribute rather than the `className` property because:
// - In SVG, `className` is read only and throws an error if assigned. Setting the `class` attribute works.
// - It’s easier to virtualize `class` since no special case mapping from `class` to `className` is needed.
// - Properties are diffed against the actual DOM node. If a third-party script or browser extension add an
//   extra class on an element, that would be removed the next time Elm renders, even if nothing changed
//   about that element. Attributes are diffed against the previous virtual DOM, making it more likely that
//   extra added classes survive for some time.
// - Properties are applied every render, even for lazy nodes, to make sure that for example `value` is up-to-date
//   (it might have been altered by the web page user by typing in some field). `Html.Attributes.class` is likely
//   one of the most used `Html.Attributes` functions in view code, and does not need to be applied every render.
//   So not doing that is a small performance win.
var _VirtualDom_elmExplorationsTestBackwardsCompatibility = typeof _Test_runThunk === 'function';


function _VirtualDom_organizeFacts(factList)
{
	var facts = {};

	// Mark all elements for virtualization of server rendered nodes – see `_VirtualDom_markerProperty`.
	facts[_VirtualDom_markerProperty] = true;

	for (; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_elmExplorationsTestBackwardsCompatibility
				? _VirtualDom_addClass(facts, 'className', value)
				: _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode, tNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode, tNode);
	}

	if (tag === 4)
	{
		return _VirtualDom_render(vNode.k, function (msg) { return eventNode(vNode.j(msg)) }, tNode);
	}

	if (tag === 0)
	{
		var domNode = _VirtualDom_doc.createTextNode(vNode.a);
		tNode.r = domNode;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, {}, vNode.d);
		tNode.r = domNode;
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		var listener = _VirtualDom_divertHrefToApp(domNode);
		domNode.elmAf = listener;
		domNode.addEventListener('click', listener);
	}

	_VirtualDom_applyFacts(domNode, eventNode, {}, vNode.d);

	if (tag === 1)
	{
		for (var kids = vNode.e, i = 0; i < kids.length; i++)
		{
			var childTNode = _VirtualDom_createTNode(undefined);
			var childDomNode = _VirtualDom_render(kids[i], eventNode, childTNode);
			tNode.s[i] = childTNode;
			_VirtualDom_appendChild(domNode, childDomNode);
		}
	}
	else
	{
		for (var kids = vNode.e, i = 0; i < kids.length; i++)
		{
			var kid = kids[i];
			var childTNode = _VirtualDom_createTNode(undefined);
			var childDomNode = _VirtualDom_render(kid.b, eventNode, childTNode);
			tNode.s[kid.a] = childTNode;
			_VirtualDom_appendChild(domNode, childDomNode);
		}
	}

	tNode.r = domNode;

	return domNode;
}

// Like `_VirtualDom_render`, but:
// - Assumes that we have already gone through diffing.
// - Only re-renders text nodes.
function _VirtualDom_renderTranslated(vNode, eventNode, tNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_renderTranslated(vNode.k, eventNode, tNode);
	}

	if (tag === 4)
	{
		return _VirtualDom_renderTranslated(vNode.k, function (msg) { return eventNode(vNode.j(msg)) }, tNode);
	}

	if (tag === 0)
	{
		var newNode = _VirtualDom_doc.createTextNode(vNode.a);
		tNode.r = newNode;
		return newNode;
	}

	return tNode.r;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, prevFacts, facts)
{
	// Since properties and attributes are sometimes linked, we need to remove old
	// ones before setting new ones. Otherwise we might set the `id` attribute and
	// then remove the `id` property, resulting in no id, for example.

	if (prevFacts.a1 !== undefined)
	{
		_VirtualDom_removeStyles(domNode, prevFacts.a1, facts.a1 || {});
	}

	// `_VirtualDom_organizeFacts` puts properties directly on the `facts` object,
	// instead of at `facts.a2` which would have been more reasonable. So
	// we pass the entire `facts` as the props, and `_VirtualDom_removeProps` needs
	// to ignore `a3` etc.
	// This results in that you can mess things up by setting properties called "a0" to "a4",
	// but it’s not a big deal.
	// We can’t fix this because of backwards compatibility with:
	// https://github.com/elm-explorations/test/blob/9669a27d84fc29175364c7a60d5d700771a2801e/src/Test/Html/Internal/ElmHtml/InternalTypes.elm#L328
	// https://github.com/dillonkearns/elm-pages/blob/fa1d0347016e20917b412de5c3657c2e6e095087/src/Test/Html/Internal/ElmHtml/InternalTypes.elm#L330
	_VirtualDom_removeProps(domNode, prevFacts, facts);

	if (prevFacts.a3 !== undefined)
	{
		_VirtualDom_removeAttrs(domNode, prevFacts.a3, facts.a3 || {});
	}

	if (prevFacts.a4 !== undefined)
	{
		_VirtualDom_removeAttrsNS(domNode, prevFacts.a4, facts.a4 || {});
	}

	// Then, apply new facts.

	if (facts.a1 !== undefined)
	{
		_VirtualDom_applyStyles(domNode, prevFacts.a1 || {}, facts.a1);
	}

	if (facts.a3 !== undefined)
	{
		_VirtualDom_applyAttrs(domNode, prevFacts.a3 || {}, facts.a3);
	}

	if (facts.a4 !== undefined)
	{
		_VirtualDom_applyAttrsNS(domNode, prevFacts.a4 || {}, facts.a4);
	}

	// Apply properties _after_ attributes. This means that if you set the same
	// thing both as a property and an attribute, the property wins. If the
	// attribute had won, the property would “win” during the next render,
	// since properties are diffed against the actual DOM node, while
	// attributes are diffed against the previous virtual node. So it's better
	// to let the property win right away.
	// See the comment at the `_VirtualDom_removeProps` call earlier in this
	// function for why we pass the entire `facts` object.
	_VirtualDom_applyProps(domNode, facts);

	// Finally, apply events. There is no separate phase for removing events.
	// Attributes and properties can't interfere with events, so it's fine.

	if (facts.a0 !== undefined || prevFacts.a0 !== undefined)
	{
		_VirtualDom_applyEvents(domNode, eventNode, facts.a0 || {});
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, prevStyles, styles)
{
	for (var key in styles)
	{
		var value = styles[key];
		if (value !== prevStyles[key])
		{
			// `.setProperty` must be used for `--custom-properties`.
			// Standard properties never start with a dash.
			// `.setProperty` requires for example 'border-radius' with a dash,
			// while both `.style['border-radius']` and `.style['borderRadius']` work.
			// Elm used to only use `.style`. In order to support existing code like
			// `Html.Attributes.style 'borderRadius' '5px'` we default to `.style`
			// and only use `.setProperty` if the property name starts with a dash.
			if (key.charCodeAt(0) === 45)
			{
				domNode.style.setProperty(key, value);
			}
			else
			{
				domNode.style[key] = value;
			}
		}
	}
}


function _VirtualDom_removeStyles(domNode, prevStyles, styles)
{
	for (var key in prevStyles)
	{
		if (!(key in styles))
		{
			// See `_VirtualDom_applyStyles`.
			if (key.charCodeAt(0) === 45)
			{
				domNode.style.removeProperty(key);
			}
			else
			{
				domNode.style[key] = '';
			}
		}
	}
}



// APPLY PROPS

function _VirtualDom_applyProps(domNode, props)
{
	for (var key in props)
	{
		// See `_VirtualDom_applyFacts` and `_VirtualDom_markerProperty` for why we need to filter these.
		if (key === 'a0' || key === 'a1' || key === 'a3' || key === 'a4' || key === _VirtualDom_markerProperty)
		{
			continue;
		}

		var value = props[key];
		// `value`, `checked`, `selected` and `selectedIndex` can all change via
		// user interactions, so for those it’s important to compare to the
		// actual DOM value. Because of that we compare against the actual DOM
		// node, rather than `prevProps`. Note that many properties are
		// normalized (to certain values, or to a full URL, for example), so if
		// you use properties they might be set on every render if you don't
		// supply the normalized form. `Html.Attributes` avoids this by
		// primarily using attributes.
		if (value !== domNode[key])
		{
			domNode[key] = value;
		}
	}
}


function _VirtualDom_removeProps(domNode, prevProps, props)
{
	for (var key in prevProps)
	{
		// See `_VirtualDom_applyFacts` and `_VirtualDom_markerProperty` for why we need to filter these.
		if (key === 'a0' || key === 'a1' || key === 'a3' || key === 'a4' || key === _VirtualDom_markerProperty)
		{
			continue;
		}

		if (!(key in props))
		{
			var value = prevProps[key];
			switch (typeof value)
			{
				// Most string properties default to the empty string.
				case 'string':
					domNode[key] = '';
					break;
				// Most boolean properties default to false.
				case 'boolean':
					domNode[key] = false;
					break;
				// For other types it's unclear what to do.
			}
			// Standard properties cannot be deleted, but it is not an error trying.
			// Non-standard properties can be deleted.
			delete domNode[key];
		}
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, prevAttrs, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (value !== prevAttrs[key])
		{
			domNode.setAttribute(key, value);
		}
	}
}


function _VirtualDom_removeAttrs(domNode, prevAttrs, attrs)
{
	for (var key in prevAttrs)
	{
		if (!(key in attrs))
		{
			domNode.removeAttribute(key);
		}
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, prevNsAttrs, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;
		var previous = prevNsAttrs[key];
		if (!previous)
		{
			domNode.setAttributeNS(namespace, key, value);
		}
		else if (previous.f !== namespace)
		{
			domNode.removeAttributeNS(previous.f, key);
			domNode.setAttributeNS(namespace, key, value);
		}
		else if (previous.o !== value)
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}


function _VirtualDom_removeAttrsNS(domNode, prevNsAttrs, nsAttrs)
{
	for (var key in prevNsAttrs)
	{
		if (!(key in nsAttrs))
		{
			domNode.removeAttributeNS(prevNsAttrs[key].f, key);
		}
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			delete allCallbacks[key];
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.p;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.p = newHandler;
				oldCallback.q = eventNode;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}

	for (key in allCallbacks)
	{
		if (!(key in events))
		{
			domNode.removeEventListener(key, allCallbacks[key]);
			delete allCallbacks[key];
		}
	}
}

function _VirtualDom_lazyUpdateEvents(domNode, eventNode)
{
	var allCallbacks = domNode.elmFs;

	if (allCallbacks)
	{
		for (var key in allCallbacks)
		{
			var oldCallback = allCallbacks[key];
			oldCallback.q = eventNode;
		}
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(initialEventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.p;
		var eventNode = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.p = initialHandler;
	callback.q = initialEventNode;

	return callback;
}



// DIFF


function _VirtualDom_diff(_x, y)
{
	// Hack to provide the new virtual dom node to `_VirtualDom_applyPatches` without
	// making breaking changes to elm/browser.
	return y;
}

function _VirtualDom_diffHelp(x, y, eventNode, tNode)
{
	if (x === y)
	{
		return {
			r: _VirtualDom_quickVisit(x, y, eventNode, tNode),
			u: false,
			v: false
		};
	}

	// Remember: When virtualizing already existing DOM, we can’t know
	// where `map` and `lazy` nodes should be, and which ones are `Keyed`.
	// So it’s important to not redraw fully when just the new virtual dom node
	// is a `map` or `lazy` or `Keyed`, to avoid unnecessary DOM changes on startup.

	while (x.$ === 4)
	{
		x = x.k;
	}

	if (y.$ === 4)
	{
		return _VirtualDom_diffHelp(x, y.k, function (msg) { return eventNode(y.j(msg)) }, tNode);
	}

	if (x.$ === 5)
	{
		if (y.$ === 5)
		{
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				// We still need to visit every node inside the lazy node, to
				// make sure that the event listeners get the current
				// `eventNode`, and to increase and reset counters. This is
				// cheaper than calling `view`, diffing and rendering at least.
				return {
					r: _VirtualDom_quickVisit(x, y, eventNode, tNode),
					u: false,
					v: false
				};
			}
			y.k = y.m();
			return _VirtualDom_diffHelp(x.k, y.k, eventNode, tNode);
		}
		else
		{
			return _VirtualDom_diffHelp(x.k, y, eventNode, tNode);
		}
	}

	if (y.$ === 5)
	{
		return _VirtualDom_diffHelp(x, y.k || (y.k = y.m()), eventNode, tNode);
	}

	var domNode = tNode.r;

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			x = _VirtualDom_upkey(x, y, tNode);
			xType = 2;
		}
		else if (xType === 2 && yType === 1)
		{
			x = _VirtualDom_dekey(x, tNode);
			xType = 1;
		}
		else
		{
			return _VirtualDom_applyPatchRedraw(x, y, eventNode, tNode);
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 0:
			if (x.a !== y.a)
			{
				// Text replaced or changed by translation plugins.
				if (!domNode.parentNode || domNode.data !== x.a)
				{
					return {
						r: domNode,
						u: true,
						v: false
					};
				}
				// Google Translate has a race condition-style bug where if you update the text
				// of a text node while it is fetching a translation for it, you’ll end up with
				// that out-of-date translation. So if we’ve ever detected a translation, it’s
				// no longer safe to update text nodes. Instead, we must replace them with new ones.
				// That’s slower, so we only switch to this method if needed.
				// See: https://issues.chromium.org/issues/393698470
				if (_VirtualDom_everTranslated)
				{
					var newNode = _VirtualDom_doc.createTextNode(y.a);
					tNode.r = newNode;
					domNode.parentNode.replaceChild(newNode, domNode);
					domNode = newNode;
				}
				else
				{
					domNode.data = y.a;
				}
			}
			return {
				r: domNode,
				u: false,
				v: false
			};

		case 1:
			return _VirtualDom_diffNodes(domNode, x, y, eventNode, tNode, _VirtualDom_diffKids);

		case 2:
			return _VirtualDom_diffNodes(domNode, x, y, eventNode, tNode, _VirtualDom_diffKeyedKids);

		case 3:
			if (x.h !== y.h)
			{
				return _VirtualDom_applyPatchRedraw(x, y, eventNode, tNode);
			}

			_VirtualDom_applyFacts(domNode, eventNode, x.d, y.d);

			var patch = y.i(x.g, y.g);
			patch && patch(domNode);

			return {
				r: domNode,
				u: false,
				v: false
			};
	}
}

// When we know that a node does not need updating, just quickly visit its children to:
// - Make sure that properties match the virtual node – they can be mutated by user actions, such as typing into an input.
//   `Html.Attributes` primarily uses attributes (not properties), so this shouldn’t take much time.
// - Update event listeners’ reference to the current `eventNode`.
function _VirtualDom_quickVisit(x, y, eventNode, tNode)
{
	switch (y.$)
	{
		case 4:
			return _VirtualDom_quickVisit(x.k, y.k, function (msg) { return eventNode(y.j(msg)) }, tNode);

		case 5:
			return _VirtualDom_quickVisit(x.k, y.k, eventNode, tNode);
	}

	var domNode = tNode.r;

	switch (y.$)
	{
		case 0:
			return domNode;

		case 1:
			_VirtualDom_applyProps(domNode, y.d);
			_VirtualDom_lazyUpdateEvents(domNode, eventNode);
			for (var xKids = x.e, yKids = y.e, i = 0; i < yKids.length; i++)
			{
				_VirtualDom_quickVisit(xKids[i], yKids[i], eventNode, tNode.s[i]);
			}
			return domNode;

		case 2:
			_VirtualDom_applyProps(domNode, y.d);
			_VirtualDom_lazyUpdateEvents(domNode, eventNode);
			for (var xKids = x.e, yKids = y.e, i = 0; i < yKids.length; i++)
			{
				var xKid = xKids[i];
				var yKid = yKids[i];
				_VirtualDom_quickVisit(xKid.b, yKid.b, eventNode, tNode.s[yKid.a]);
			}
			return domNode;

		case 3:
			_VirtualDom_applyProps(domNode, y.d);
			_VirtualDom_lazyUpdateEvents(domNode, eventNode);
			return domNode;
	}
}

function _VirtualDom_diffNodes(domNode, x, y, eventNode, tNode, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		return _VirtualDom_applyPatchRedraw(x, y, eventNode, tNode);
	}

	_VirtualDom_applyFacts(domNode, eventNode, x.d, y.d);

	var translated = diffKids(domNode, x, y, eventNode, tNode);

	// If at least one kid was detected to have been translated (by Google Translate for example),
	// we need to go through all kids and actual DOM node children once more. If a text node
	// has been replaced by another with translated text, we don’t know _which_ text node it has
	// been replace by. We have to rerender _all_ text inside the element. This has the side benefit
	// of increasing the likelihood of getting a well-formed sentence after the translator re-translates
	// the text. Since different languages have different word order, it’s the best to translate
	// whole sentences at the minimum. It’s difficult to heuristically find a sentence or paragraph
	// though. “All the text directly inside this element” is the best we’ve got so far.
	if (translated)
	{
		_VirtualDom_everTranslated = true;

		for (var current = null, kids = y.e, i = kids.length - 1, j = domNode.childNodes.length - 1; i >= 0; i--)
		{
			var kid = kids[i];
			var vNode = y.$ === 2 ? kid.b : kid;

			// `child` is going to be one of:
			// - For text nodes: A new text node that isn’t inserted into the DOM.
			// - For other nodes: The already existing DOM node. An extension
			//   might have removed it, though, or moved it to another parent.
			var child = _VirtualDom_renderTranslated(vNode, eventNode, tNode.s[y.$ === 2 ? kid.a : i]);

			if (child.parentNode === domNode)
			{
				// Go through the actual children of `domNode` until we hit `child`,
				// which we just checked for sure is a child of `domNode`. We know
				// that all “our” kids are in the correct order.
				for (; j >= 0; j--)
				{
					current = domNode.childNodes[j];
					if (current === child)
					{
						j--;
						break;
					}
					// Any element we come across until we find `child` must be created by others,
					// or be text nodes created by us but abandoned in `_VirtualDom_renderTranslated`.
					// Remove all text nodes, and all font tags (most likely created by Google Translate).
					if (current.nodeType === 3 || current.localName === 'font')
					{
						domNode.removeChild(current);
					}
				}
			}
			else
			{
				// Most likely, we are inserting a new text node here.
				// It could also be an element (re-)moved by an extension.
				_VirtualDom_insertBefore(domNode, child, current);
				current = child;
			}
		}

		// If there are more elements before our first kid, go through them as well like above.
		for (; j >= 0; j--)
		{
			current = domNode.childNodes[j];
			if (current.nodeType === 3 || current.localName === 'font')
			{
				domNode.removeChild(current);
			}
		}
	}

	return {
		r: domNode,
		u: false,
		v: false
	};
}



// DIFF KIDS


function _VirtualDom_diffKids(parentDomNode, xParent, yParent, eventNode, tNode)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	var translated = false;
	var previousSibling = null;

	// PAIRWISE DIFF COMMON KIDS

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var diffReturn = _VirtualDom_diffHelp(xKids[i], yKids[i], eventNode, tNode.s[i]);
		var domNode = diffReturn.r;

		if (diffReturn.u)
		{
			translated = true;
		}

		if (diffReturn.v)
		{
			_VirtualDom_insertAfter(parentDomNode, domNode, previousSibling);
			previousSibling = domNode;
		}
		// An extension might have removed an element we have rendered before,
		// or moved it to another parent. In such cases, `parentDomNode.insertBefore(x, domNode)`
		// would throw errors. Keep the previous reference element in those cases – that should still
		// result in the correct element order, just with some element missing.
		else if (domNode.parentNode === parentDomNode)
		{
			previousSibling = domNode;
		}
	}

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		for (var i = yLen; i < xLen; i++)
		{
			_VirtualDom_remove(tNode.s[i].r);
			delete tNode.s[i];
		}
	}
	else if (xLen < yLen)
	{
		for (var i = xLen; i < yLen; i++)
		{
			var y = yKids[i];
			var childTNode = _VirtualDom_createTNode(undefined);
			var domNode = _VirtualDom_render(y, eventNode, childTNode);
			tNode.s[i] = childTNode;
			_VirtualDom_appendChild(parentDomNode, domNode);
		}
	}

	return translated;
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(parentDomNode, xParent, yParent, eventNode, tNode)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xKidsMap = xParent.t;
	var yKidsMap = yParent.t;

	var xIndexLower = 0;
	var yIndexLower = 0;
	var xIndexUpper = xKids.length - 1;
	var yIndexUpper = yKids.length - 1;

	var domNodeLower = null;
	var domNodeUpper = null;

	var translated = false;

	var handleDiffReturn = function (diffReturn, upper)
	{
		var domNode = diffReturn.r;

		if (diffReturn.u)
		{
			translated = true;
		}

		if (diffReturn.v)
		{
			if (upper)
			{
				_VirtualDom_insertBefore(parentDomNode, domNode, domNodeUpper);
				domNodeUpper = domNode;
			}
			else
			{
				_VirtualDom_insertAfter(parentDomNode, domNode, domNodeLower);
				domNodeLower = domNode;
			}
		}
		// An extension might have removed an element we have rendered before,
		// or moved it to another parent. In such cases, `parentDomNode.insertBefore(x, domNode)`
		// and `parentDomNode.moveBefore(x, domNode)` would throw errors. Keep the
		// previous reference element in those cases – that should still result in the correct
		// element order, just with some element missing.
		else if (domNode.parentNode === parentDomNode)
		{
			if (upper)
			{
				domNodeUpper = domNode;
			}
			else
			{
				domNodeLower = domNode;
			}
		}
	};

	while (true)
	{
		// Consume from the start until we get stuck.
		while (xIndexLower <= xIndexUpper && yIndexLower <= yIndexUpper)
		{
			var xKid = xKids[xIndexLower];
			var yKid = yKids[yIndexLower];
			var xKey = xKid.a;
			var yKey = yKid.a;
			var x = xKid.b;
			var y = yKid.b;

			if (xKey === yKey)
			{
				var diffReturn = _VirtualDom_diffHelp(x, y, eventNode, tNode.s[yKey]);
				xIndexLower++;
				yIndexLower++;
				handleDiffReturn(diffReturn, false);
				continue;
			}

			var xMoved = false;

			if (xKey in yKidsMap)
			{
				xMoved = true;
			}
			else
			{
				_VirtualDom_remove(tNode.s[xKey].r);
				delete tNode.s[xKey];
				xIndexLower++;
			}

			if (yKey in xKidsMap)
			{
				if (xMoved)
				{
					break;
				}
			}
			else
			{
				var childTNode = _VirtualDom_createTNode(undefined);
				var domNode = _VirtualDom_render(y, eventNode, childTNode);
				tNode.s[yKey] = childTNode;
				_VirtualDom_insertAfter(parentDomNode, domNode, domNodeLower);
				yIndexLower++;
				domNodeLower = domNode;
			}
		}

		// Consume from the end until we get stuck.
		while (xIndexUpper > xIndexLower && yIndexUpper > yIndexLower)
		{
			var xKid = xKids[xIndexUpper];
			var yKid = yKids[yIndexUpper];
			var xKey = xKid.a;
			var yKey = yKid.a;
			var x = xKid.b;
			var y = yKid.b;

			if (xKey === yKey)
			{
				var diffReturn = _VirtualDom_diffHelp(x, y, eventNode, tNode.s[yKey]);
				xIndexUpper--;
				yIndexUpper--;
				handleDiffReturn(diffReturn, true);
				continue;
			}

			var xMoved = false;

			if (xKey in yKidsMap)
			{
				xMoved = true;
			}
			else
			{
				_VirtualDom_remove(tNode.s[xKey].r);
				delete tNode.s[xKey];
				xIndexUpper--;
			}

			if (yKey in xKidsMap)
			{
				if (xMoved)
				{
					break;
				}
			}
			else
			{
				var childTNode = _VirtualDom_createTNode(undefined);
				var domNode = _VirtualDom_render(y, eventNode, childTNode);
				tNode.s[yKey] = childTNode;
				_VirtualDom_insertBefore(parentDomNode, domNode, domNodeUpper);
				yIndexUpper--;
				domNodeUpper = domNode;
			}
		}

		var swapped = false;

		// Check if the start or end can be unstuck by a swap.
		if (xIndexLower < xIndexUpper && yIndexLower < yIndexUpper)
		{
			var xKidLower = xKids[xIndexLower];
			var yKidLower = yKids[yIndexLower];
			var xKidUpper = xKids[xIndexUpper];
			var yKidUpper = yKids[yIndexUpper];

			var xKeyLower = xKidLower.a;
			var yKeyLower = yKidLower.a;
			var xKeyUpper = xKidUpper.a;
			var yKeyUpper = yKidUpper.a;

			if (xKeyLower === yKeyUpper)
			{
				var diffReturn = _VirtualDom_diffHelp(xKidLower.b, yKidUpper.b, eventNode, tNode.s[yKeyUpper]);
				xIndexLower++;
				yIndexUpper--;
				_VirtualDom_moveBefore(parentDomNode, diffReturn.r, domNodeUpper);
				handleDiffReturn(diffReturn, true);
				swapped = true;
			}

			if (xKeyUpper === yKeyLower)
			{
				var diffReturn = _VirtualDom_diffHelp(xKidUpper.b, yKidLower.b, eventNode, tNode.s[yKeyLower]);
				yIndexLower++;
				xIndexUpper--;
				_VirtualDom_moveAfter(parentDomNode, diffReturn.r, domNodeLower);
				handleDiffReturn(diffReturn, false);
				swapped = true;
			}
		}

		// If no swap, stop consuming from start and end.
		if (!swapped)
		{
			break;
		}
	}

	// For the remaining items in the new virtual DOM, diff with the corresponding
	// old virtual DOM node (if any) and move it into the correct place.
	// This might result in more moves than technically needed, but:
	// - Moving nodes isn’t that slow. Diffing algorithms aren’t free either.
	// - In browsers supporting `.moveBefore()` unnecessary moves have no unwanted side effects.
	// - Elm has never had a “perfect” implementation for Keyed, and this should not
	//   be worse than the previous implementation.
	for (; yIndexLower <= yIndexUpper; yIndexLower++)
	{
		var yKid = yKids[yIndexLower];
		var yKey = yKid.a;
		var y = yKid.b;
		if (yKey in xKidsMap)
		{
			var x = xKidsMap[yKey];
			var diffReturn = _VirtualDom_diffHelp(x, y, eventNode, tNode.s[yKey]);
			_VirtualDom_moveAfter(parentDomNode, diffReturn.r, domNodeLower);
			handleDiffReturn(diffReturn, false);
		}
		else
		{
			var childTNode = _VirtualDom_createTNode(undefined);
			var domNode = _VirtualDom_render(y, eventNode, childTNode);
			tNode.s[yKey] = childTNode;
			_VirtualDom_insertAfter(parentDomNode, domNode, domNodeLower);
			domNodeLower = domNode;
		}
	}

	// Remove the remaining old virtual DOM nodes that aren’t present in the new virtual DOM.
	for (; xIndexLower <= xIndexUpper; xIndexLower++)
	{
		var xKid = xKids[xIndexLower];
		var xKey = xKid.a;
		if (!(xKey in yKidsMap)) {
			_VirtualDom_remove(tNode.s[xKid.a].r);
			delete tNode.s[xKid.a];
		}
	}

	return translated;
}

var _VirtualDom_POSTFIX = '_elmW6BL';

function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, newVirtualNode, eventNode)
{
	// To avoid making breaking changes to elm/browser, we store the tNode on
	// the root DOM node instead of returning it.
	var tNode = rootDomNode.elmTree;

	var diffReturn = _VirtualDom_diffHelp(oldVirtualNode, newVirtualNode, eventNode, tNode);
	// We can’t do anything about `diffReturn.u` or
	// `diffReturn.v` here, because we don’t know the parent of the
	// root node. Note that `rootDomNode.parentNode` cannot be used, because if
	// the root node is a text node and it has been translated, it is most
	// likely replaced by other nodes (so the original node is not attached to
	// the DOM anymore). Returning `Html.text` at the top level of `view` and
	// expecting it to be translatable is a bit of an edge case anyway.
	var newDomNode = diffReturn.r;

	if (newDomNode !== rootDomNode)
	{
		delete rootDomNode.elmTree;
		newDomNode.elmTree = tNode;
	}

	return newDomNode;
}

function _VirtualDom_applyPatchRedraw(x, y, eventNode, tNode)
{
	var domNode = tNode.r;
	var parentNode = domNode.parentNode;
	var isTextNode = domNode.nodeType === 3;
	var newNode = _VirtualDom_render(y, eventNode, tNode);

	// An extension might have removed the element. In this case, we are redrawing because `x` and `y`
	// have changed a lot, implying that the structure has changed significantly, and that they can’t
	// be diffed normally. This means that the extension probably meant to remove the old element, but
	// not the new one, so return that this element is missing so that it can be re-inserted into the
	// parent. An example of this is Google Translate: It removes our text nodes and replaces them.
	// Later we might want to replace that text node with some element.
	if (parentNode)
	{
		parentNode.replaceChild(newNode, domNode);
		return {
			r: newNode,
			u: isTextNode && domNode.data !== x.a,
			v: false
		}
	}
	else
	{
		return {
			r: newNode,
			u: isTextNode,
			v: true
		}
	}
}

/*
This is a mapping between attribute names and their corresponding boolean properties,
and only the ones where the attribute name is different from the property name
(usually in casing – attributes are case insensitive, and returned lowercase).

The mapping currently only lists the ones that have dedicated functions in elm/html.

There are more though! Running the following code in the console gives more results:

[...new Set(Object.getOwnPropertyNames(window).filter(d => d.startsWith('HTML') || d === 'Node' || d === 'Element' || d === 'EventTarget').flatMap(d => {c = window[d]; m = c.name.match(/^HTML(\w+)Element$/); e = document.createElement(m ? m[1].replace('Anchor', 'a').replace('Paragraph', 'p').replace('Image', 'img').replace('Media', 'video').replace(/^([DOU])List$/, '$1l').toLowerCase() : 'div'); return Object.getOwnPropertyNames(c.prototype).filter(n => typeof e[n] === 'boolean')}))].filter(n => /[A-Z]/.test(n)).sort()

Potential candidates to support (should probably add to elm/html first):
disablePictureInPicture – video
playsInline – video
formNoValidate – button, input

Not useful with Elm:
noModule – script
shadowRootClonable – template
shadowRootDelegatesFocus – template
shadowRootSerializable – template

Legacy/deprecated:
allowFullscreen – iframe (use allow="fullscreen" instead)
allowPaymentRequest – iframe (use allow="payment" instead)
noHref - area (image maps)
noResize – frame (not iframe)
noShade – hr
trueSpeed – marquee

Special:
defaultChecked
defaultMuted
defaultSelected

No corresponding attribute:
disableRemotePlayback
isConnected
isContentEditable
preservesPitch
sharedStorageWritable
willValidate

Unclear:
adAuctionHeaders
browsingTopics

Regarding the special ones: `<input checked>` results in `.defaultChecked ===
true`. Similarly, setting `input.defaultChecked = true` results in
`input.outerHTML === '<input checked="">'`. `input.checked = true` does _not_
result in an attribute though: `.checked` has no corresponding attribute.
However, when serializing `Html.input [ Html.Attributes.checked True ] []` to
HTML, `<input checked>` is the most reasonable choice. So when virtualizing, we
actually want to turn the `checked` attribute back into a boolean "checked"
property in Elm (even if according to the DOM, it's `.defaultChecked`). Same
thing for `muted` and `selected`.
*/
var _VirtualDom_camelCaseBoolProperties = {
	novalidate: 'noValidate',
	readonly: 'readOnly',
	ismap: 'isMap'
};

// Used for server side rendering to keep track of which elements to
// virtualize. This is added to _all_ nodes (except text nodes) in
// `_VirtualDom_organizeFacts`. Server side rendering renders _all_ string and
// boolean facts as attributes, including this one. `_VirtualDom_applyProps`
// and `_VirtualDom_removeProps` _ignore_ this property, in order not to
// clutter the browser dev tools. `_VirtualDom_virtualize` only virtualizes
// children with this attribute. This way it knows which elements are “ours”
// and which were inserted by third-party scripts (before the virtualization
// took place). The root node is allowed not to have this attribute though, in
// order not to force everyone to put this attribute on the node they mount the
// Elm app on. During the first render after virtualization, we remove this
// attribute from all elements, to unclutter the browser console. That happens
// via `_VirtualDom_virtualize` virtualizing it as an _attribute_ (not a
// property) which, when compared to the result of `view`, is diffed for
// removal.
var _VirtualDom_markerProperty = 'data-elm';

function _VirtualDom_virtualize(node)
{
	// The debugger has always done `_VirtualDom_virtualize(document)` instead of
	// `_VirtualDom_virtualize(document.body)` by mistake. To be backwards compatible
	// with elm/browser, support that here.
	if (node === _VirtualDom_doc)
	{
		node = _VirtualDom_doc.body;
	}

	if (node.elmTree)
	{
		// The `console.error` lets the user more easily identify which node they passed.
		console.error('node.elmTree already exists:', node.elmTree, node);
		throw new Error('node.elmTree already exists!');
	}

	var tNode = _VirtualDom_createTNode(node);

	// Fall back to a text node as backwards compatibility. Elm has always
	// supported mounting onto any node, even comment nodes. Text nodes,
	// comment nodes, CDATA sections and processing instructions all implement
	// the `CharacterData` abstract interface, so representing them as a text
	// node should be fine. The whole document, doctypes and document fragments
	// are also nodes, but they are increasingly silly to render into and have
	// never worked with Elm.
	var vNode = _VirtualDom_virtualizeHelp(node, tNode) || _VirtualDom_text('');

	node.elmTree = tNode;

	return vNode;
}

function _VirtualDom_virtualizeHelp(node, tNode)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return undefined;
	}


	// ELEMENT NODES

	var tag = node.localName;
	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;

		// The `style` attribute and `node.style` are linked. While `node.style` contains
		// every single CSS property, it’s possible to loop over only the styles that have
		// been set via `node.style.length`. Unfortunately, `node.style` expands shorthand
		// properties and normalizes values. For example, `padding: 0` is turned into
		// `padding-top: 0px; padding-bottom: 0px; ...`.
		// The best bet is actually parsing the styles ourselves. Naively splitting on `;`
		// is not 100 % correct, for example it won’t work for `content: ";"`. It will work
		// in 99 % of cases though, since putting a semicolon in a value isn’t that common.
		// And even in those cases, nothing will break. We’ll just apply a few styles
		// unnecessarily at init.
		if (name === "style")
		{
			var parts = value.split(";");
			for (var j = parts.length; j--; )
			{
				var part = parts[j];
				var index = part.indexOf(":");
				if (index !== -1)
				{
					var cssKey = part.slice(0, index).trim();
					var cssValue = part.slice(index + 1).trim();
					attrList = _List_Cons(A2(_VirtualDom_style, cssKey, cssValue), attrList);
				}
			}
			continue;
		}

		var namespaceURI = attr.namespaceURI;
		var propertyName = _VirtualDom_camelCaseBoolProperties[name] || name;
		var propertyValue = node[propertyName];
		// Turning attributes into virtual DOM representations is not an exact science.
		// If someone runs an Elm `view` function and then serializes it to HTML, we need to guess:
		//
		// - how they chose to serialize it
		// - what the most likely virtual DOM representation is
		//
		// In elm/html, the convention is to use attributes rather than properties where possible,
		// which is good for virtualization – we can just turn most HTML attributes we find as-is
		// into virtual DOM attributes. But when we encounter `foo="bar"` we can’t know if it was
		// created using `Html.Attributes.attribute "foo" "bar"` or
		// `Html.Attributes.property "foo" (Json.Encode.string "bar")`.
		//
		// It's not the end of the world if we guess wrong, though, it just leads to a bit of
		// unnecessary DOM mutations on the first render.
		//
		// Do we need to use any of the functions in the “XSS ATTACK VECTOR CHECKS”
		// section while virtualizing? I don’t think so, because they will already
		// have executed at this point, and the first render will remove any disallowed
		// attributes.
		attrList = _List_Cons(
			// `Html.Attributes.value` sets the `.value` property to a string, because that’s the
			// only way to set the value of an input element. The `.value` property has no corresponding
			// attribute; the `value` attribute maps to the `.defaultValue` property. But when serializing,
			// the most likely way to do it is to serialize the `.value` property to the `value` attribute.
			name === 'value'
				? A2(_VirtualDom_property, name, value)
				:
			// Try to guess if the attribute comes from one of the functions
			// implemented using `boolProperty` in `Html.Attributes`.
			// See `Html.Attributes.spellcheck` for that exception.
			typeof propertyValue === 'boolean' && name !== 'spellcheck'
				? A2(_VirtualDom_property, propertyName, propertyValue)
				:
			// Otherwise, guess that it is an attribute. The user might have used `Html.Attributes.property`,
			// but there’s no way for us to know that.
			namespaceURI
				? A3(_VirtualDom_attributeNS, namespaceURI, name, value)
				: A2(_VirtualDom_attribute, name, value),
			attrList
		);
	}

	var namespace =
		node.namespaceURI === 'http://www.w3.org/1999/xhtml'
			? undefined
			: node.namespaceURI;
	var kidList = [];

	// To create a text area with default text in HTML:
	// - correct: <textarea>default text</textarea>
	// - wrong: <textarea value="default text"></textarea> (value="default text" does nothing.)
	// In the DOM, that becomes an `HTMLTextAreaElement`, with `.value === "default text"`.
	// It contains a single text node with the text `"default text"` too.
	// When the user types into the text area, `.value` changes, but the inner text node stays unchanged.
	// In Elm, you need to use `Html.textarea [ Html.Attributes.value myValue ] []` to be able to set the value.
	// All in all, this means that the most useful virtualization is:
	// - Skip any children (most likely a single text node), because the Elm code most likely set none.
	// - Pick up `.value`, even though it wasn’t set as an attribute in HTML – but most likely is a property set by the Elm code.
	// Note that in <textarea>, HTML isn’t parsed as usual – it is more of a plain text element.
	if (node.localName === 'textarea')
	{
		attrList = _List_Cons(
			A2(_VirtualDom_property, 'value', node.value),
			attrList
		);
	}
	else
	{
		for (var kids = node.childNodes, len = kids.length, i = 0; i < len; i++)
		{
			var kid = kids[i];

			// Only virtualize “our” elements – see `_VirtualDom_markerProperty`.
			if (kid.nodeType === 1 && !kid.hasAttribute(_VirtualDom_markerProperty))
			{
				continue;
			}

			var childTNode = _VirtualDom_createTNode(kid);
			var kidNode = _VirtualDom_virtualizeHelp(kid, childTNode);
			// `kidNode` is `undefined` for comment nodes – skip those. This allows
			// server side rendering to insert comments between two text nodes to
			// preserve them being parsed as two nodes, not as just one with the
			// text from both.
			if (kidNode)
			{
				var length = kidList.push(kidNode);
				tNode.s[length - 1] = childTNode;
			}
		}

		if (_VirtualDom_divertHrefToApp && node.localName === 'a')
		{
			var listener = _VirtualDom_divertHrefToApp(node);
			node.elmAf = listener;
			node.addEventListener('click', listener);
		}
	}

	// This returns the same structure as `_VirtualDom_nodeNS`, but avoids
	// calling that function, because then we can avoid constructing an
	// Elm list, reversing it, and then having `_VirtualDom_nodeNS` convert
	// that to a JS array.
	return {
		$: 1,
		c: tag,
		d: _VirtualDom_organizeFacts(attrList),
		e: kidList,
		f: namespace,
		b: 0
	};
}

function _VirtualDom_upkey(node, keyedNode, tNode) {
	var kids = node.e;
	var keyedKids = keyedNode.e;
	var len = kids.length;
	var keyedLen = keyedKids.length;
	var newKeyedKids = new Array(len);
	var newKidsMap = Object.create(null);
	var newChildren = Object.create(null);
	for (var i = 0; i < len; i++)
	{
		var kid = kids[i];
		var key;
		if (i < keyedLen)
		{
			key = keyedKids[i].a;
		}
		else
		{
			key = i + _VirtualDom_POSTFIX;
			// Handle duplicate keys by adding a postfix.
			while (key in newKidsMap)
			{
				key += _VirtualDom_POSTFIX;
			}
		}
		newKeyedKids[i] = _Utils_Tuple2(key, kid);
		newKidsMap[key] = kid;
		newChildren[key] = tNode.s[i];
	}
	tNode.s = newChildren;

	return {
		$: 2,
		c: node.c,
		d: node.d,
		e: newKeyedKids,
		t: newKidsMap,
		f: node.f,
		b: node.b
	};
}

function _VirtualDom_dekey(keyedNode, tNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	var newChildren = Object.create(null);
	for (var i = 0; i < len; i++)
	{
		var keyedKid = keyedKids[i];
		kids[i] = keyedKid.b;
		newChildren[i] = tNode.s[keyedKid.a];
	}
	tNode.s = newChildren;

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}

function _VirtualDom_removeAllEventListeners(rootDomNode)
{
	var tNode = rootDomNode.elmTree;

	// Allow mounting another Elm app on this DOM node.
	delete rootDomNode.elmTree;

	_VirtualDom_removeAllEventListenersHelp(tNode);
}

function _VirtualDom_removeAllEventListenersHelp(tNode)
{
	var domNode = tNode.r;
	var children = tNode.s;

	// Allow the DOM nodes to be virtualized by another Elm app.
	domNode.setAttribute(_VirtualDom_markerProperty, '');

	if (domNode.elmAf)
	{
		domNode.removeEventListener('click', domNode.elmAf);
		delete domNode.elmAf;
	}

	var allCallbacks = domNode.elmFs;
	if (allCallbacks)
	{
		for (var key in allCallbacks)
		{
			domNode.removeEventListener(key, allCallbacks[key]);
		}
		delete domNode.elmFs;
	}

	for (var key in children)
	{
		_VirtualDom_removeAllEventListenersHelp(children[key]);
	}
}

// Used by `_Debugger_document` to remove the corner when shutting down.
function _VirtualDom_removeLastElmChild(rootDomNode)
{
	var tNode = rootDomNode.elmTree;
	var children = tNode.s;
	var childNodes = rootDomNode.childNodes;
	for (var i = childNodes.length - 1; i >= 0; i--)
	{
		var childDomNode = childNodes[i];
		for (var key in children)
		{
			if (childDomNode === children[key].r)
			{
				rootDomNode.removeChild(childDomNode);
				delete children[key];
				return;
			}
		}
	}
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F3(function(impl, flagDecoder, debugMetadata)
{
	var init = function(args)
	{
		return _Platform_initialize(
			flagDecoder,
			args,
			// These three arguments are used by old versions of _Platform_initialize.
			// Newer versions ignore them.
			impl.init,
			impl.update,
			impl.subscriptions,
			function(sendToApp, initialModel, platformInitializeWillDoInitialDraw) {
				/**_UNUSED/
				var domNode = args['node'];
				//*/
				/**/
				var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
				//*/
				var currNode = _VirtualDom_virtualize(domNode);

				var stepper = _Browser_makeAnimator(function(model)
				{
					var nextNode = impl.view(model);
					var patches = _VirtualDom_diff(currNode, nextNode);
					domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
					currNode = nextNode;
				});

				stepper.shutdown = function()
				{
					// Older versions of elm/virtual-dom does not provide this function.
					if (typeof _VirtualDom_removeAllEventListeners === 'function')
					{
						_VirtualDom_removeAllEventListeners(domNode);
					}

					return domNode;
				};

				// The initial draw used to be a side effect of `stepperBuilder`.
				// Newer versions of `_Platform_initialize` do that instead.
				// Older versions don’t send the `platformInitializeWillDoInitialDraw`
				// parameter, which means that we need to do it here for compatibility.
				if (!platformInitializeWillDoInitialDraw)
				{
					stepper(initialModel, true);
				}

				return stepper;
			},
			// Only used by newer versions of _Platform_initialize.
			impl
		);
	};

	/**/
	init.hotReloadData = {
		impl: impl,
		platform_effectManagers: _Platform_effectManagers,
		scheduler_enqueue: _Scheduler_enqueue
	};
	//*/

	return init;
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F3(function(impl, flagDecoder, debugMetadata)
{
	var init = function(args)
	{
		return _Platform_initialize(
			flagDecoder,
			args,
			// These three arguments are used by old versions of _Platform_initialize.
			// Newer versions ignore them.
			impl.init,
			impl.update,
			impl.subscriptions,
			function(sendToApp, initialModel, platformInitializeWillDoInitialDraw) {
				var divertHrefToApp = impl.setup && impl.setup(sendToApp)
				var title = _VirtualDom_doc.title;
				var bodyNode = _VirtualDom_doc.body;
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var currNode = _VirtualDom_virtualize(bodyNode);
				_VirtualDom_divertHrefToApp = 0;

				var stepper = _Browser_makeAnimator(function(model)
				{
					_VirtualDom_divertHrefToApp = divertHrefToApp;
					var doc = impl.view(model);
					var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
					var patches = _VirtualDom_diff(currNode, nextNode);
					bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
					currNode = nextNode;
					_VirtualDom_divertHrefToApp = 0;
					(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
				});

				stepper.shutdown = function()
				{
					// Older versions of elm/virtual-dom does not provide this function.
					if (typeof _VirtualDom_removeAllEventListeners === 'function')
					{
						_VirtualDom_removeAllEventListeners(bodyNode);
					}

					if (impl.shutdown)
					{
						impl.shutdown();
					}

					return bodyNode;
				};

				// The initial draw used to be a side effect of `stepperBuilder`.
				// Newer versions of `_Platform_initialize` do that instead.
				// Older versions don’t send the `platformInitializeWillDoInitialDraw`
				// parameter, which means that we need to do it here for compatibility.
				if (!platformInitializeWillDoInitialDraw)
				{
					stepper(initialModel, true);
				}

				return stepper;
			},
			// Only used by newer versions of _Platform_initialize.
			impl
		);
	}

	/**/
	init.hotReloadData = {
		impl: impl,
		platform_effectManagers: _Platform_effectManagers,
		scheduler_enqueue: _Scheduler_enqueue
	};
	//*/

	return init;
});



// ANIMATION


var _Browser_requestAnimationFrame_queue = {};
var _Browser_inAnimationFrame = false;
var _Browser_pendingAnimationFrame = false;
var _Browser_requestAnimationFrame_id = 0;

function _Browser_cancelAnimationFrame(id)
{
	delete _Browser_requestAnimationFrame_queue[id];
}

function _Browser_requestAnimationFrame(callback)
{
	var id = _Browser_requestAnimationFrame_id;
	_Browser_requestAnimationFrame_id++;
	_Browser_requestAnimationFrame_queue[id] = callback;
	if (!_Browser_pendingAnimationFrame)
	{
		_Browser_pendingAnimationFrame = true;
		_Browser_requestAnimationFrame_raw(function() {
			_Browser_pendingAnimationFrame = false;
			_Browser_inAnimationFrame = true;
			var maxId = _Browser_requestAnimationFrame_id;
			for (var id2 in _Browser_requestAnimationFrame_queue)
			{
				if (id2 >= maxId)
				{
					break;
				}
				var callback = _Browser_requestAnimationFrame_queue[id2];
				delete _Browser_requestAnimationFrame_queue[id2];
				callback();
			}
			_Browser_inAnimationFrame = false;
		});
	}
	return id;
}

var _Browser_requestAnimationFrame_raw =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };

function _Browser_makeAnimator(draw)
{
	var model;

	// Whether `draw` is currently running. `draw` can cause side effects:
	// If the user renders a custom element, they can dispatch an event in
	// its `connectedCallback`, which happens synchronously. That causes
	// `update` to run while we’re in the middle of drawing, which then
	// causes another call to the returned function below. We can’t start
	// another draw while before the first one is finished.
	var drawing = false;

	// Whether we have already requested an animation frame for drawing.
	var pendingFrame = false;

	// Whether we have already requested to draw right after the current draw has finished.
	var pendingSync = false;

	function drawHelp()
	{
		// If we’re already drawing, wait until that draw is done.
		if (drawing)
		{
			pendingSync = true;
			return;
		}

		pendingFrame = false;
		pendingSync = false;
		drawing = true;
		draw(model);
		drawing = false;

		if (pendingSync)
		{
			drawHelp();
		}
	}

	function updateIfNeeded()
	{
		if (pendingFrame)
		{
			drawHelp();
		}
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		// When using `Browser.Events.onAnimationFrame` we already are
		// in an animation frame, so draw straight away. Otherwise we’ll
		// be drawing one frame late all the time.
		if (isSync || _Browser_inAnimationFrame)
		{
			drawHelp();
		}
		else if (!pendingFrame)
		{
			pendingFrame = true;
			_Browser_requestAnimationFrame(updateIfNeeded);
		}
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var key = function() { key.a(impl.onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download') && domNode.hasAttribute('href'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(impl.onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		shutdown: function()
		{
			_Browser_window.removeEventListener('popstate', key);
			_Browser_window.removeEventListener('hashchange', key);
			// Allow the app to be garbage collected.
			key.a = function() {};
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		// Unnecessary-looking wrapper functions are needed during development
		// for hot reloading. In production, we optimize slightly by omitting them.
		/**_UNUSED/
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
		//*/
		/**/
		view: function(model) { return impl.view(model); },
		update: F2(function(msg, model) { return A2(impl.update, msg, model); }),
		subscriptions: function(model) { return impl.subscriptions(model); }
		//*/
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$Basics$False = {$: 'False'};
var $author$project$ElmDev$Debugger$ModelDelta = {$: 'ModelDelta'};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $author$project$ElmDev$Debugger$RuntimeEvent = function (a) {
	return {$: 'RuntimeEvent', a: a};
};
var $author$project$ElmDev$Debugger$TimelineView = {$: 'TimelineView'};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
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
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
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
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$ElmDev$Debugger$fromRuntime = _Platform_incomingPort('fromRuntime', $elm$json$Json$Decode$value);
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$ElmDev$Debugger$toRuntime = _Platform_outgoingPort('toRuntime', $elm$core$Basics$identity);
var $author$project$ElmDev$Debugger$setOpen = function (open) {
	return $author$project$ElmDev$Debugger$toRuntime(
		$elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('setOpen')),
					_Utils_Tuple2(
					'open',
					$elm$json$Json$Encode$bool(open))
				])));
};
var $author$project$ElmDev$Debugger$Unknown = function (a) {
	return {$: 'Unknown', a: a};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $author$project$ElmDev$Debugger$copyText = function (string) {
	return $author$project$ElmDev$Debugger$toRuntime(
		$elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('copyText')),
					_Utils_Tuple2(
					'text',
					$elm$json$Json$Encode$string(string))
				])));
};
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
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
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $author$project$ElmDev$Debugger$nextSelected = F2(
	function (current, event) {
		switch (event.$) {
			case 'Init':
				var initEvent = event.a;
				return $elm$core$Maybe$Just(initEvent.id);
			case 'Update':
				var updateEvent = event.a;
				return $elm$core$Maybe$Just(updateEvent.id);
			case 'Frame':
				return current;
			default:
				return current;
		}
	});
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Basics$not = _Basics_not;
var $author$project$ElmDev$Debugger$popOutDebugger = $author$project$ElmDev$Debugger$toRuntime(
	$elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string('popOut'))
			])));
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
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
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
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
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
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
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
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
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
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
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$remove = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A2($elm$core$Dict$remove, key, dict));
	});
var $author$project$ElmDev$Debugger$Frame = function (a) {
	return {$: 'Frame', a: a};
};
var $author$project$ElmDev$Debugger$Init = function (a) {
	return {$: 'Init', a: a};
};
var $author$project$ElmDev$Debugger$RuntimeDebugLog = function (a) {
	return {$: 'RuntimeDebugLog', a: a};
};
var $author$project$ElmDev$Debugger$RuntimeDebuggerEvent = function (a) {
	return {$: 'RuntimeDebuggerEvent', a: a};
};
var $author$project$ElmDev$Debugger$RuntimePerformance = function (a) {
	return {$: 'RuntimePerformance', a: a};
};
var $author$project$ElmDev$Debugger$Update = function (a) {
	return {$: 'Update', a: a};
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $author$project$ElmDev$Debugger$DebugLog = F5(
	function (logId, label, source, value, timestamp) {
		return {label: label, logId: logId, source: source, timestamp: timestamp, value: value};
	});
var $author$project$ElmDev$Debugger$ArrayValue = function (a) {
	return {$: 'ArrayValue', a: a};
};
var $author$project$ElmDev$Debugger$BoolValue = function (a) {
	return {$: 'BoolValue', a: a};
};
var $author$project$ElmDev$Debugger$CharValue = function (a) {
	return {$: 'CharValue', a: a};
};
var $author$project$ElmDev$Debugger$ConstructorValue = F2(
	function (a, b) {
		return {$: 'ConstructorValue', a: a, b: b};
	});
var $author$project$ElmDev$Debugger$DictEntry = F2(
	function (key, value) {
		return {key: key, value: value};
	});
var $author$project$ElmDev$Debugger$DictValue = function (a) {
	return {$: 'DictValue', a: a};
};
var $author$project$ElmDev$Debugger$Field = F2(
	function (name, value) {
		return {name: name, value: value};
	});
var $author$project$ElmDev$Debugger$ListValue = function (a) {
	return {$: 'ListValue', a: a};
};
var $author$project$ElmDev$Debugger$NumberValue = function (a) {
	return {$: 'NumberValue', a: a};
};
var $author$project$ElmDev$Debugger$OpaqueValue = function (a) {
	return {$: 'OpaqueValue', a: a};
};
var $author$project$ElmDev$Debugger$RecordValue = function (a) {
	return {$: 'RecordValue', a: a};
};
var $author$project$ElmDev$Debugger$SetValue = function (a) {
	return {$: 'SetValue', a: a};
};
var $author$project$ElmDev$Debugger$StringValue = function (a) {
	return {$: 'StringValue', a: a};
};
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$ElmDev$Debugger$debugValueByKind = function (kind) {
	switch (kind) {
		case 'bool':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$ElmDev$Debugger$BoolValue,
				A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$bool));
		case 'number':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$ElmDev$Debugger$NumberValue,
				A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$string));
		case 'string':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$ElmDev$Debugger$StringValue,
				A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$string));
		case 'char':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$ElmDev$Debugger$CharValue,
				A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$string));
		case 'record':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$ElmDev$Debugger$RecordValue,
				A2(
					$elm$json$Json$Decode$field,
					'fields',
					$elm$json$Json$Decode$list(
						$author$project$ElmDev$Debugger$cyclic$fieldDecoder())));
		case 'constructor':
			return A3(
				$elm$json$Json$Decode$map2,
				$author$project$ElmDev$Debugger$ConstructorValue,
				A2(
					$elm$json$Json$Decode$field,
					'name',
					$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
				A2(
					$elm$json$Json$Decode$field,
					'args',
					$elm$json$Json$Decode$list(
						$author$project$ElmDev$Debugger$cyclic$debugValueDecoder())));
		case 'list':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$ElmDev$Debugger$ListValue,
				A2(
					$elm$json$Json$Decode$field,
					'items',
					$elm$json$Json$Decode$list(
						$author$project$ElmDev$Debugger$cyclic$debugValueDecoder())));
		case 'array':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$ElmDev$Debugger$ArrayValue,
				A2(
					$elm$json$Json$Decode$field,
					'items',
					$elm$json$Json$Decode$list(
						$author$project$ElmDev$Debugger$cyclic$debugValueDecoder())));
		case 'set':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$ElmDev$Debugger$SetValue,
				A2(
					$elm$json$Json$Decode$field,
					'items',
					$elm$json$Json$Decode$list(
						$author$project$ElmDev$Debugger$cyclic$debugValueDecoder())));
		case 'dict':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$ElmDev$Debugger$DictValue,
				A2(
					$elm$json$Json$Decode$field,
					'entries',
					$elm$json$Json$Decode$list(
						$author$project$ElmDev$Debugger$cyclic$dictEntryDecoder())));
		case 'opaque':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$ElmDev$Debugger$OpaqueValue,
				A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
		default:
			return $elm$json$Json$Decode$succeed(
				$author$project$ElmDev$Debugger$OpaqueValue('<unknown>'));
	}
};
function $author$project$ElmDev$Debugger$cyclic$fieldDecoder() {
	return A3(
		$elm$json$Json$Decode$map2,
		$author$project$ElmDev$Debugger$Field,
		A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
		A2(
			$elm$json$Json$Decode$field,
			'value',
			$author$project$ElmDev$Debugger$cyclic$debugValueDecoder()));
}
function $author$project$ElmDev$Debugger$cyclic$dictEntryDecoder() {
	return A3(
		$elm$json$Json$Decode$map2,
		$author$project$ElmDev$Debugger$DictEntry,
		A2(
			$elm$json$Json$Decode$field,
			'key',
			$author$project$ElmDev$Debugger$cyclic$debugValueDecoder()),
		A2(
			$elm$json$Json$Decode$field,
			'value',
			$author$project$ElmDev$Debugger$cyclic$debugValueDecoder()));
}
function $author$project$ElmDev$Debugger$cyclic$debugValueDecoder() {
	return A2(
		$elm$json$Json$Decode$andThen,
		$author$project$ElmDev$Debugger$debugValueByKind,
		A2($elm$json$Json$Decode$field, 'kind', $elm$json$Json$Decode$string));
}
try {
	var $author$project$ElmDev$Debugger$fieldDecoder = $author$project$ElmDev$Debugger$cyclic$fieldDecoder();
	$author$project$ElmDev$Debugger$cyclic$fieldDecoder = function () {
		return $author$project$ElmDev$Debugger$fieldDecoder;
	};
	var $author$project$ElmDev$Debugger$dictEntryDecoder = $author$project$ElmDev$Debugger$cyclic$dictEntryDecoder();
	$author$project$ElmDev$Debugger$cyclic$dictEntryDecoder = function () {
		return $author$project$ElmDev$Debugger$dictEntryDecoder;
	};
	var $author$project$ElmDev$Debugger$debugValueDecoder = $author$project$ElmDev$Debugger$cyclic$debugValueDecoder();
	$author$project$ElmDev$Debugger$cyclic$debugValueDecoder = function () {
		return $author$project$ElmDev$Debugger$debugValueDecoder;
	};
} catch ($) {
	throw 'Some top-level definitions from `ElmDev.Debugger` are causing infinite recursion:\n\n  ┌─────┐\n  │    fieldDecoder\n  │     ↓\n  │    dictEntryDecoder\n  │     ↓\n  │    debugValueDecoder\n  │     ↓\n  │    debugValueByKind\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$ElmDev$Debugger$LogSource = F5(
	function (moduleName, file, source, line, column) {
		return {column: column, file: file, line: line, moduleName: moduleName, source: source};
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$map5 = _Json_map5;
var $author$project$ElmDev$Debugger$logSourceDecoder = A6(
	$elm$json$Json$Decode$map5,
	$author$project$ElmDev$Debugger$LogSource,
	A2($elm$json$Json$Decode$field, 'module', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'file', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'source', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['region', 'start', 'line']),
		$elm$json$Json$Decode$int),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['region', 'start', 'column']),
		$elm$json$Json$Decode$int));
var $author$project$ElmDev$Debugger$debugLogDecoder = A6(
	$elm$json$Json$Decode$map5,
	$author$project$ElmDev$Debugger$DebugLog,
	A2($elm$json$Json$Decode$field, 'logId', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'source',
		$elm$json$Json$Decode$nullable($author$project$ElmDev$Debugger$logSourceDecoder)),
	A2($elm$json$Json$Decode$field, 'value', $author$project$ElmDev$Debugger$debugValueDecoder),
	A2($elm$json$Json$Decode$field, 'timestamp', $elm$json$Json$Decode$float));
var $author$project$ElmDev$Debugger$FrameEvent = F3(
	function (id, duration, renderDuration) {
		return {duration: duration, id: id, renderDuration: renderDuration};
	});
var $elm$json$Json$Decode$map3 = _Json_map3;
var $author$project$ElmDev$Debugger$frameEventDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$ElmDev$Debugger$FrameEvent,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'duration', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'renderDuration', $elm$json$Json$Decode$float));
var $author$project$ElmDev$Debugger$InitEvent = F2(
	function (id, model) {
		return {id: id, model: model};
	});
var $author$project$ElmDev$Debugger$initEventDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$ElmDev$Debugger$InitEvent,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'model', $author$project$ElmDev$Debugger$debugValueDecoder));
var $author$project$ElmDev$Debugger$PerformanceSnapshot = function (lazy) {
	return {lazy: lazy};
};
var $author$project$ElmDev$Debugger$LazyStat = F9(
	function (id, name, arity, calls, renders, hits, avgRender, maxRender, argStats) {
		return {argStats: argStats, arity: arity, avgRender: avgRender, calls: calls, hits: hits, id: id, maxRender: maxRender, name: name, renders: renders};
	});
var $author$project$ElmDev$Debugger$LazyArgStat = F3(
	function (index, comparisons, misses) {
		return {comparisons: comparisons, index: index, misses: misses};
	});
var $author$project$ElmDev$Debugger$lazyArgStatDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$ElmDev$Debugger$LazyArgStat,
	A2($elm$json$Json$Decode$field, 'index', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'comparisons', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'misses', $elm$json$Json$Decode$int));
var $elm$json$Json$Decode$map8 = _Json_map8;
var $author$project$ElmDev$Debugger$lazyStatDecoder = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (build, argStats) {
			return build(argStats);
		}),
	A9(
		$elm$json$Json$Decode$map8,
		$author$project$ElmDev$Debugger$LazyStat,
		A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'arity', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'calls', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'renders', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'hits', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'avgRender', $elm$json$Json$Decode$float),
		A2($elm$json$Json$Decode$field, 'maxRender', $elm$json$Json$Decode$float)),
	A2(
		$elm$json$Json$Decode$field,
		'argStats',
		$elm$json$Json$Decode$list($author$project$ElmDev$Debugger$lazyArgStatDecoder)));
var $author$project$ElmDev$Debugger$performanceSnapshotDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$ElmDev$Debugger$PerformanceSnapshot,
	A2(
		$elm$json$Json$Decode$field,
		'lazy',
		$elm$json$Json$Decode$list($author$project$ElmDev$Debugger$lazyStatDecoder)));
var $author$project$ElmDev$Debugger$UpdateEvent = F6(
	function (id, message, rawMessage, modelBefore, modelAfter, duration) {
		return {duration: duration, id: id, message: message, modelAfter: modelAfter, modelBefore: modelBefore, rawMessage: rawMessage};
	});
var $elm$json$Json$Decode$map6 = _Json_map6;
var $author$project$ElmDev$Debugger$updateEventDecoder = A7(
	$elm$json$Json$Decode$map6,
	$author$project$ElmDev$Debugger$UpdateEvent,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'message', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'rawMessage', $author$project$ElmDev$Debugger$debugValueDecoder),
	A2($elm$json$Json$Decode$field, 'modelBefore', $author$project$ElmDev$Debugger$debugValueDecoder),
	A2($elm$json$Json$Decode$field, 'modelAfter', $author$project$ElmDev$Debugger$debugValueDecoder),
	A2($elm$json$Json$Decode$field, 'duration', $elm$json$Json$Decode$float));
var $author$project$ElmDev$Debugger$runtimeMessageDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (eventType) {
		switch (eventType) {
			case 'init':
				return A2(
					$elm$json$Json$Decode$map,
					A2($elm$core$Basics$composeL, $author$project$ElmDev$Debugger$RuntimeDebuggerEvent, $author$project$ElmDev$Debugger$Init),
					$author$project$ElmDev$Debugger$initEventDecoder);
			case 'update':
				return A2(
					$elm$json$Json$Decode$map,
					A2($elm$core$Basics$composeL, $author$project$ElmDev$Debugger$RuntimeDebuggerEvent, $author$project$ElmDev$Debugger$Update),
					$author$project$ElmDev$Debugger$updateEventDecoder);
			case 'frame':
				return A2(
					$elm$json$Json$Decode$map,
					A2($elm$core$Basics$composeL, $author$project$ElmDev$Debugger$RuntimeDebuggerEvent, $author$project$ElmDev$Debugger$Frame),
					$author$project$ElmDev$Debugger$frameEventDecoder);
			case 'performance':
				return A2($elm$json$Json$Decode$map, $author$project$ElmDev$Debugger$RuntimePerformance, $author$project$ElmDev$Debugger$performanceSnapshotDecoder);
			case 'debugLog':
				return A2($elm$json$Json$Decode$map, $author$project$ElmDev$Debugger$RuntimeDebugLog, $author$project$ElmDev$Debugger$debugLogDecoder);
			default:
				return A2(
					$elm$json$Json$Decode$map,
					A2($elm$core$Basics$composeL, $author$project$ElmDev$Debugger$RuntimeDebuggerEvent, $author$project$ElmDev$Debugger$Unknown),
					$elm$json$Json$Decode$value);
		}
	},
	A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string));
var $author$project$ElmDev$Debugger$setInspectMode = function (enabled) {
	return $author$project$ElmDev$Debugger$toRuntime(
		$elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('setInspectMode')),
					_Utils_Tuple2(
					'enabled',
					$elm$json$Json$Encode$bool(enabled))
				])));
};
var $author$project$ElmDev$Debugger$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'RuntimeEvent':
				var value = msg.a;
				var _v1 = A2($elm$json$Json$Decode$decodeValue, $author$project$ElmDev$Debugger$runtimeMessageDecoder, value);
				if (_v1.$ === 'Ok') {
					switch (_v1.a.$) {
						case 'RuntimeDebuggerEvent':
							var event = _v1.a.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										events: _Utils_ap(
											model.events,
											_List_fromArray(
												[event])),
										selected: A2($author$project$ElmDev$Debugger$nextSelected, model.selected, event)
									}),
								$elm$core$Platform$Cmd$none);
						case 'RuntimePerformance':
							var snapshot = _v1.a.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{lazyStats: snapshot.lazy}),
								$elm$core$Platform$Cmd$none);
						default:
							var logEntry = _v1.a.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										debugLogs: _Utils_ap(
											model.debugLogs,
											_List_fromArray(
												[logEntry]))
									}),
								$elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								events: _Utils_ap(
									model.events,
									_List_fromArray(
										[
											$author$project$ElmDev$Debugger$Unknown(value)
										]))
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'Select':
				var id = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							selected: $elm$core$Maybe$Just(id)
						}),
					$elm$core$Platform$Cmd$none);
			case 'Scrub':
				var raw = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							selected: $elm$core$String$toInt(raw)
						}),
					$elm$core$Platform$Cmd$none);
			case 'SelectView':
				var viewMode = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{viewMode: viewMode}),
					$elm$core$Platform$Cmd$none);
			case 'SelectModelView':
				var modelView = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{modelView: modelView}),
					$elm$core$Platform$Cmd$none);
			case 'SelectLogSource':
				var source = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{selectedLogSource: source}),
					$elm$core$Platform$Cmd$none);
			case 'ToggleOpen':
				var nextOpen = !model.open;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{open: nextOpen}),
					$author$project$ElmDev$Debugger$setOpen(nextOpen));
			case 'ToggleInspectMode':
				var nextInspectMode = !model.inspectMode;
				return nextInspectMode ? _Utils_Tuple2(
					_Utils_update(
						model,
						{inspectMode: true, open: false}),
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$ElmDev$Debugger$setInspectMode(true),
								$author$project$ElmDev$Debugger$setOpen(false)
							]))) : _Utils_Tuple2(
					_Utils_update(
						model,
						{inspectMode: false}),
					$author$project$ElmDev$Debugger$setInspectMode(false));
			case 'PopOut':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{open: true}),
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$ElmDev$Debugger$setOpen(true),
								$author$project$ElmDev$Debugger$popOutDebugger
							])));
			case 'ToggleCollapse':
				var path = msg.a;
				var isCollapsed = msg.b;
				return isCollapsed ? _Utils_Tuple2(
					_Utils_update(
						model,
						{
							collapsed: A2($elm$core$Set$remove, path, model.collapsed),
							expanded: A2($elm$core$Set$insert, path, model.expanded)
						}),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{
							collapsed: A2($elm$core$Set$insert, path, model.collapsed),
							expanded: A2($elm$core$Set$remove, path, model.expanded)
						}),
					$elm$core$Platform$Cmd$none);
			case 'ToggleMessagePayload':
				var id = msg.a;
				return A2($elm$core$Set$member, id, model.messageDrawers) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{
							messageDrawers: A2($elm$core$Set$remove, id, model.messageDrawers)
						}),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{
							messageDrawers: A2($elm$core$Set$insert, id, model.messageDrawers)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var string = msg.a;
				return _Utils_Tuple2(
					model,
					$author$project$ElmDev$Debugger$copyText(string));
		}
	});
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$div = _VirtualDom_node('div');
var $author$project$ElmDev$Debugger$ToggleOpen = {$: 'ToggleOpen'};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$ElmDev$Debugger$minimizedStyles = _List_fromArray(
	[
		$elm$html$Html$Events$onClick($author$project$ElmDev$Debugger$ToggleOpen),
		A2($elm$html$Html$Attributes$style, 'width', '100%'),
		A2($elm$html$Html$Attributes$style, 'height', '100%'),
		A2($elm$html$Html$Attributes$style, 'border', '1px solid #f6a400'),
		A2($elm$html$Html$Attributes$style, 'border-radius', '999px'),
		A2($elm$html$Html$Attributes$style, 'background', 'linear-gradient(90deg, #261700, #f6a400)'),
		A2($elm$html$Html$Attributes$style, 'color', '#140c00'),
		A2($elm$html$Html$Attributes$style, 'font', '800 13px ui-monospace, monospace'),
		A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
		A2($elm$html$Html$Attributes$style, 'box-shadow', '0 8px 24px rgba(0,0,0,.35), 0 0 18px rgba(246,164,0,.35)'),
		A2($elm$html$Html$Attributes$style, 'letter-spacing', '0.06em')
	]);
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $author$project$ElmDev$Debugger$selectableEvents = function (events) {
	return A2(
		$elm$core$List$filter,
		function (event) {
			switch (event.$) {
				case 'Frame':
					return false;
				case 'Unknown':
					return false;
				default:
					return true;
			}
		},
		events);
};
var $author$project$ElmDev$Debugger$shellStyles = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'font-family', 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace'),
		A2($elm$html$Html$Attributes$style, 'width', '100%'),
		A2($elm$html$Html$Attributes$style, 'height', '100%'),
		A2($elm$html$Html$Attributes$style, 'display', 'flex'),
		A2($elm$html$Html$Attributes$style, 'flex-direction', 'column'),
		A2($elm$html$Html$Attributes$style, 'background', 'linear-gradient(135deg, rgba(26, 20, 10, 0.98), rgba(8, 9, 10, 0.98))'),
		A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
		A2($elm$html$Html$Attributes$style, 'box-sizing', 'border-box'),
		A2($elm$html$Html$Attributes$style, 'overflow', 'hidden'),
		A2($elm$html$Html$Attributes$style, 'border', '1px solid #f6a400'),
		A2($elm$html$Html$Attributes$style, 'border-radius', '14px'),
		A2($elm$html$Html$Attributes$style, 'box-shadow', 'inset 0 0 0 1px rgba(255, 188, 46, .16), 0 0 36px rgba(246, 164, 0, .18)')
	]);
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$ElmDev$Debugger$PopOut = {$: 'PopOut'};
var $author$project$ElmDev$Debugger$headerButton = F2(
	function (msg, label) {
		return A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(msg),
					A2($elm$html$Html$Attributes$style, 'background', 'rgba(246,164,0,.12)'),
					A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
					A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.7)'),
					A2($elm$html$Html$Attributes$style, 'border-radius', '999px'),
					A2($elm$html$Html$Attributes$style, 'font', 'inherit'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
					A2($elm$html$Html$Attributes$style, 'padding', '3px 10px')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(label)
				]));
	});
var $author$project$ElmDev$Debugger$ToggleInspectMode = {$: 'ToggleInspectMode'};
var $author$project$ElmDev$Debugger$inspectButtonStyles = function (active) {
	return _List_fromArray(
		[
			$elm$html$Html$Events$onClick($author$project$ElmDev$Debugger$ToggleInspectMode),
			A2(
			$elm$html$Html$Attributes$style,
			'background',
			active ? 'rgba(246,164,0,.9)' : 'rgba(246,164,0,.12)'),
			A2(
			$elm$html$Html$Attributes$style,
			'color',
			active ? '#140c00' : '#ffd77a'),
			A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.7)'),
			A2($elm$html$Html$Attributes$style, 'border-radius', '999px'),
			A2($elm$html$Html$Attributes$style, 'font', 'inherit'),
			A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
			A2($elm$html$Html$Attributes$style, 'padding', '3px 10px')
		]);
};
var $author$project$ElmDev$Debugger$viewDebuggerHeader = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'padding', '12px 14px'),
				A2($elm$html$Html$Attributes$style, 'border-bottom', '1px solid rgba(246, 164, 0, .45)'),
				A2($elm$html$Html$Attributes$style, 'background', 'linear-gradient(90deg, rgba(246,164,0,.24), rgba(246,164,0,.04))'),
				A2($elm$html$Html$Attributes$style, 'font-weight', '800'),
				A2($elm$html$Html$Attributes$style, 'letter-spacing', '0.08em'),
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
				A2($elm$html$Html$Attributes$style, 'justify-content', 'space-between'),
				A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase'),
				A2($elm$html$Html$Attributes$style, 'gap', '10px')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text('Elm Dev Debugger'),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'flex'),
						A2($elm$html$Html$Attributes$style, 'gap', '8px'),
						A2($elm$html$Html$Attributes$style, 'align-items', 'center')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						$author$project$ElmDev$Debugger$inspectButtonStyles(model.inspectMode),
						_List_fromArray(
							[
								$elm$html$Html$text('Inspect')
							])),
						A2($author$project$ElmDev$Debugger$headerButton, $author$project$ElmDev$Debugger$PopOut, 'Pop Out'),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$ElmDev$Debugger$ToggleOpen),
								A2($elm$html$Html$Attributes$style, 'background', 'rgba(246,164,0,.12)'),
								A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
								A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.7)'),
								A2($elm$html$Html$Attributes$style, 'border-radius', '999px'),
								A2($elm$html$Html$Attributes$style, 'font', 'inherit'),
								A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
								A2($elm$html$Html$Attributes$style, 'padding', '3px 10px')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Minimize')
							]))
					]))
			]));
};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$ElmDev$Debugger$rate = F2(
	function (part, total) {
		return (total <= 0) ? 0 : (part / total);
	});
var $author$project$ElmDev$Debugger$lazyScore = function (stat) {
	return ((A2($author$project$ElmDev$Debugger$rate, stat.renders, stat.calls) * 100) + stat.maxRender) + (stat.avgRender * 4);
};
var $author$project$ElmDev$Debugger$compareLazyStats = F2(
	function (left, right) {
		return A2(
			$elm$core$Basics$compare,
			$author$project$ElmDev$Debugger$lazyScore(right),
			$author$project$ElmDev$Debugger$lazyScore(left));
	});
var $elm$core$List$sortWith = _List_sortWith;
var $author$project$ElmDev$Debugger$rankLazyStats = function (stats) {
	return A2($elm$core$List$sortWith, $author$project$ElmDev$Debugger$compareLazyStats, stats);
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$core$Basics$round = _Basics_round;
var $author$project$ElmDev$Debugger$formatMs = function (duration) {
	return $elm$core$String$fromFloat(
		$elm$core$Basics$round(duration * 100) / 100) + 'ms';
};
var $author$project$ElmDev$Debugger$formatPercent = function (value_) {
	return $elm$core$String$fromFloat(
		$elm$core$Basics$round(value_ * 1000) / 10) + '%';
};
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$ElmDev$Debugger$lazyDiagnosis = function (stat) {
	var renderRate = A2($author$project$ElmDev$Debugger$rate, stat.renders, stat.calls);
	return (stat.calls < 5) ? 'warming up' : (((renderRate >= 0.95) && (stat.avgRender < 1)) ? 'likely unnecessary' : ((renderRate >= 0.95) ? 'always rendering' : (((renderRate <= 0.25) && (stat.avgRender >= 1)) ? 'helping' : ((renderRate <= 0.5) ? 'mostly helping' : 'mixed'))));
};
var $author$project$ElmDev$Debugger$lazyArgEqualityBackground = function (equalityRate) {
	return (equalityRate <= 0.05) ? 'rgba(246,70,0,.28)' : ((equalityRate <= 0.5) ? 'rgba(246,164,0,.18)' : 'rgba(246,164,0,.07)');
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$title = $elm$html$Html$Attributes$stringProperty('title');
var $author$project$ElmDev$Debugger$viewLazyArgStat = function (argStat) {
	var equalityRate = 1 - A2($author$project$ElmDev$Debugger$rate, argStat.misses, argStat.comparisons);
	return A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$title(
				'arg ' + ($elm$core$String$fromInt(argStat.index) + (' kept the same reference ' + ($elm$core$String$fromInt(argStat.comparisons - argStat.misses) + (' of ' + ($elm$core$String$fromInt(argStat.comparisons) + ' comparisons')))))),
				A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.28)'),
				A2($elm$html$Html$Attributes$style, 'border-radius', '999px'),
				A2($elm$html$Html$Attributes$style, 'padding', '1px 6px'),
				A2(
				$elm$html$Html$Attributes$style,
				'background',
				$author$project$ElmDev$Debugger$lazyArgEqualityBackground(equalityRate)),
				A2($elm$html$Html$Attributes$style, 'white-space', 'nowrap')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				$elm$core$String$fromInt(argStat.index) + (': ' + $author$project$ElmDev$Debugger$formatPercent(equalityRate)))
			]));
};
var $author$project$ElmDev$Debugger$viewLazyArgStats = function (argStats) {
	return $elm$core$List$isEmpty(argStats) ? $elm$html$Html$text('none') : A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				A2($elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
				A2($elm$html$Html$Attributes$style, 'gap', '4px')
			]),
		A2($elm$core$List$map, $author$project$ElmDev$Debugger$viewLazyArgStat, argStats));
};
var $author$project$ElmDev$Debugger$viewMetricHtml = F2(
	function (label, value_) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'background', 'rgba(8,9,10,.86)'),
					A2($elm$html$Html$Attributes$style, 'padding', '10px 12px'),
					A2($elm$html$Html$Attributes$style, 'min-width', '0')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'font-size', '10px'),
							A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase'),
							A2($elm$html$Html$Attributes$style, 'letter-spacing', '0.14em'),
							A2($elm$html$Html$Attributes$style, 'color', '#9f7a31'),
							A2($elm$html$Html$Attributes$style, 'margin-bottom', '4px')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'font-size', '14px'),
							A2($elm$html$Html$Attributes$style, 'font-weight', '900'),
							A2($elm$html$Html$Attributes$style, 'color', '#ffe6aa'),
							A2($elm$html$Html$Attributes$style, 'overflow-wrap', 'anywhere')
						]),
					_List_fromArray(
						[value_]))
				]));
	});
var $author$project$ElmDev$Debugger$viewMetricText = F2(
	function (label, value_) {
		return A2(
			$author$project$ElmDev$Debugger$viewMetricHtml,
			label,
			$elm$html$Html$text(value_));
	});
var $author$project$ElmDev$Debugger$viewLazyStat = function (stat) {
	var renderRate = A2($author$project$ElmDev$Debugger$rate, stat.renders, stat.calls);
	var hitRate = A2($author$project$ElmDev$Debugger$rate, stat.hits, stat.calls);
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.28)'),
				A2($elm$html$Html$Attributes$style, 'border-radius', '12px'),
				A2($elm$html$Html$Attributes$style, 'background', 'rgba(13,10,5,.52)'),
				A2($elm$html$Html$Attributes$style, 'overflow', 'hidden')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'padding', '12px 14px'),
						A2($elm$html$Html$Attributes$style, 'display', 'grid'),
						A2($elm$html$Html$Attributes$style, 'grid-template-columns', '1fr max-content'),
						A2($elm$html$Html$Attributes$style, 'gap', '16px'),
						A2($elm$html$Html$Attributes$style, 'align-items', 'baseline'),
						A2($elm$html$Html$Attributes$style, 'background', 'rgba(246,164,0,.08)')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'font-size', '17px'),
										A2($elm$html$Html$Attributes$style, 'font-weight', '900'),
										A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
										A2($elm$html$Html$Attributes$style, 'overflow-wrap', 'anywhere')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(stat.name)
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'color', '#9f7a31'),
										A2($elm$html$Html$Attributes$style, 'font-size', '12px'),
										A2($elm$html$Html$Attributes$style, 'margin-top', '3px')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										'lazy' + ($elm$core$String$fromInt(stat.arity) + (' #' + $elm$core$String$fromInt(stat.id))))
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'color', '#f6a400'),
								A2($elm$html$Html$Attributes$style, 'font-weight', '900')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								$author$project$ElmDev$Debugger$lazyDiagnosis(stat))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'grid'),
						A2($elm$html$Html$Attributes$style, 'grid-template-columns', 'repeat(auto-fit, minmax(110px, 1fr))'),
						A2($elm$html$Html$Attributes$style, 'gap', '1px'),
						A2($elm$html$Html$Attributes$style, 'background', 'rgba(246,164,0,.16)')
					]),
				_List_fromArray(
					[
						A2(
						$author$project$ElmDev$Debugger$viewMetricText,
						'Calls',
						$elm$core$String$fromInt(stat.calls)),
						A2(
						$author$project$ElmDev$Debugger$viewMetricText,
						'Cache hit rate',
						$author$project$ElmDev$Debugger$formatPercent(hitRate) + (' / ' + ($elm$core$String$fromInt(stat.hits) + ' avoided'))),
						A2(
						$author$project$ElmDev$Debugger$viewMetricText,
						'Render time',
						'avg ' + ($author$project$ElmDev$Debugger$formatMs(stat.avgRender) + (' / max ' + $author$project$ElmDev$Debugger$formatMs(stat.maxRender)))),
						A2(
						$author$project$ElmDev$Debugger$viewMetricHtml,
						'Arg equality',
						$author$project$ElmDev$Debugger$viewLazyArgStats(stat.argStats))
					]))
			]));
};
var $author$project$ElmDev$Debugger$viewLazyStats = function (stats) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'overflow', 'auto'),
				A2($elm$html$Html$Attributes$style, 'flex', '1'),
				A2($elm$html$Html$Attributes$style, 'min-width', '0'),
				A2($elm$html$Html$Attributes$style, 'padding', '22px')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'flex'),
						A2($elm$html$Html$Attributes$style, 'align-items', 'baseline'),
						A2($elm$html$Html$Attributes$style, 'justify-content', 'space-between'),
						A2($elm$html$Html$Attributes$style, 'gap', '20px'),
						A2($elm$html$Html$Attributes$style, 'margin-bottom', '18px')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'font-size', '11px'),
										A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase'),
										A2($elm$html$Html$Attributes$style, 'letter-spacing', '0.16em'),
										A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('live performance')
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'font-size', '24px'),
										A2($elm$html$Html$Attributes$style, 'font-weight', '900'),
										A2($elm$html$Html$Attributes$style, 'color', '#ffd77a')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Lazy calls')
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'color', '#9f7a31'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'right')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Rendered means the lazy thunk actually ran. Avoided renders reused the previous lazy subtree.')
							]))
					])),
				$elm$core$List$isEmpty(stats) ? A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'padding', '24px'),
						A2($elm$html$Html$Attributes$style, 'border', '1px dashed rgba(246,164,0,.35)'),
						A2($elm$html$Html$Attributes$style, 'border-radius', '12px'),
						A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('No Html.Lazy calls observed yet.')
					])) : A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'grid'),
						A2($elm$html$Html$Attributes$style, 'gap', '10px')
					]),
				A2(
					$elm$core$List$map,
					$author$project$ElmDev$Debugger$viewLazyStat,
					$author$project$ElmDev$Debugger$rankLazyStats(stats)))
			]));
};
var $author$project$ElmDev$Debugger$logSourceKey = function (logEntry) {
	var _v0 = logEntry.source;
	if (_v0.$ === 'Just') {
		var source = _v0.a;
		return source.source;
	} else {
		return '<unknown source>';
	}
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
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
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$ElmDev$Debugger$addUnique = F2(
	function (value_, values) {
		return A2($elm$core$List$member, value_, values) ? values : _Utils_ap(
			values,
			_List_fromArray(
				[value_]));
	});
var $author$project$ElmDev$Debugger$logSources = function (logs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (logEntry, sources) {
				return A2(
					$author$project$ElmDev$Debugger$addUnique,
					$author$project$ElmDev$Debugger$logSourceKey(logEntry),
					sources);
			}),
		_List_Nil,
		logs);
};
var $author$project$ElmDev$Debugger$fieldPrefix = function (index) {
	return (!index) ? '' : ', ';
};
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$ElmDev$Debugger$viewAtom = function (label) {
	return A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'color', '#ffe6aa'),
				A2($elm$html$Html$Attributes$style, 'overflow-wrap', 'anywhere')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(label)
			]));
};
var $author$project$ElmDev$Debugger$CopyToClipboard = function (a) {
	return {$: 'CopyToClipboard', a: a};
};
var $author$project$ElmDev$Debugger$viewString = function (string) {
	var rendered = A2(
		$elm$json$Json$Encode$encode,
		0,
		$elm$json$Json$Encode$string(string));
	return ($elm$core$String$length(string) > 120) ? A2(
		$elm$html$Html$button,
		_List_fromArray(
			[
				$elm$html$Html$Events$onClick(
				$author$project$ElmDev$Debugger$CopyToClipboard(string)),
				$elm$html$Html$Attributes$title('Copy full string'),
				A2($elm$html$Html$Attributes$style, 'background', 'rgba(246,164,0,.08)'),
				A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.28)'),
				A2($elm$html$Html$Attributes$style, 'border-radius', '4px'),
				A2($elm$html$Html$Attributes$style, 'color', '#ffe6aa'),
				A2($elm$html$Html$Attributes$style, 'font', 'inherit'),
				A2($elm$html$Html$Attributes$style, 'cursor', 'copy'),
				A2($elm$html$Html$Attributes$style, 'padding', '1px 4px'),
				A2($elm$html$Html$Attributes$style, 'text-align', 'left'),
				A2($elm$html$Html$Attributes$style, 'overflow-wrap', 'anywhere')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				A2($elm$core$String$left, 120, rendered) + '...')
			])) : $author$project$ElmDev$Debugger$viewAtom(rendered);
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$ElmDev$Debugger$viewInlineDebugValue = function (value_) {
	_v0$8:
	while (true) {
		switch (value_.$) {
			case 'BoolValue':
				var bool = value_.a;
				return $elm$core$Maybe$Just(
					$author$project$ElmDev$Debugger$viewAtom(
						bool ? 'True' : 'False'));
			case 'NumberValue':
				var number = value_.a;
				return $elm$core$Maybe$Just(
					$author$project$ElmDev$Debugger$viewAtom(number));
			case 'StringValue':
				var string = value_.a;
				return $elm$core$Maybe$Just(
					$author$project$ElmDev$Debugger$viewString(string));
			case 'CharValue':
				var _char = value_.a;
				return $elm$core$Maybe$Just(
					$author$project$ElmDev$Debugger$viewAtom('\'' + (_char + '\'')));
			case 'OpaqueValue':
				var label = value_.a;
				return $elm$core$Maybe$Just(
					$author$project$ElmDev$Debugger$viewAtom(label));
			case 'ConstructorValue':
				if (!value_.b.b) {
					var maybeName = value_.a;
					return $elm$core$Maybe$Just(
						$author$project$ElmDev$Debugger$viewAtom(
							A2($elm$core$Maybe$withDefault, 'Tuple', maybeName)));
				} else {
					var maybeName = value_.a;
					var args = value_.b;
					var inlineArgs = A2($elm$core$List$filterMap, $author$project$ElmDev$Debugger$viewInlineDebugValue, args);
					return _Utils_eq(
						$elm$core$List$length(inlineArgs),
						$elm$core$List$length(args)) ? $elm$core$Maybe$Just(
						A2(
							$elm$html$Html$span,
							_List_Nil,
							A2(
								$elm$core$List$cons,
								A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
											A2($elm$html$Html$Attributes$style, 'font-weight', '800')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($elm$core$Maybe$withDefault, 'Tuple', maybeName))
										])),
								A2(
									$elm$core$List$concatMap,
									function (arg) {
										return _List_fromArray(
											[
												$elm$html$Html$text(' '),
												arg
											]);
									},
									inlineArgs)))) : $elm$core$Maybe$Nothing;
				}
			case 'RecordValue':
				if (value_.a.b && (!value_.a.b.b)) {
					var _v1 = value_.a;
					var field = _v1.a;
					return A2(
						$elm$core$Maybe$map,
						function (inlineField) {
							return A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('{ '),
										inlineField,
										$elm$html$Html$text(' }')
									]));
						},
						$author$project$ElmDev$Debugger$viewInlineField(field));
				} else {
					break _v0$8;
				}
			default:
				break _v0$8;
		}
	}
	return $elm$core$Maybe$Nothing;
};
var $author$project$ElmDev$Debugger$viewInlineField = function (field) {
	return A2(
		$elm$core$Maybe$map,
		function (inlineValue) {
			return A2(
				$elm$html$Html$span,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'color', '#f6a400')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(field.name + ': ')
							])),
						inlineValue
					]));
		},
		$author$project$ElmDev$Debugger$viewInlineDebugValue(field.value));
};
var $author$project$ElmDev$Debugger$constructorArgsAreInline = function (args) {
	return _Utils_eq(
		$elm$core$List$length(
			A2($elm$core$List$filterMap, $author$project$ElmDev$Debugger$viewInlineDebugValue, args)),
		$elm$core$List$length(args));
};
var $author$project$ElmDev$Debugger$isMultilineValue = function (value_) {
	switch (value_.$) {
		case 'RecordValue':
			var fields = value_.a;
			return $elm$core$List$length(fields) > 1;
		case 'ConstructorValue':
			var args = value_.b;
			return (!$author$project$ElmDev$Debugger$constructorArgsAreInline(args)) && (!$elm$core$List$isEmpty(args));
		case 'ListValue':
			var items = value_.a;
			return !$elm$core$List$isEmpty(items);
		case 'ArrayValue':
			var items = value_.a;
			return !$elm$core$List$isEmpty(items);
		case 'SetValue':
			var items = value_.a;
			return !$elm$core$List$isEmpty(items);
		case 'DictValue':
			var entries = value_.a;
			return !$elm$core$List$isEmpty(entries);
		default:
			return false;
	}
};
var $author$project$ElmDev$Debugger$ToggleCollapse = F2(
	function (a, b) {
		return {$: 'ToggleCollapse', a: a, b: b};
	});
var $author$project$ElmDev$Debugger$defaultOpenDepth = 2;
var $author$project$ElmDev$Debugger$branchIsCollapsed = F3(
	function (model, depth, path) {
		return A2($elm$core$Set$member, path, model.collapsed) || ((_Utils_cmp(depth, $author$project$ElmDev$Debugger$defaultOpenDepth) > -1) && (!A2($elm$core$Set$member, path, model.expanded)));
	});
var $author$project$ElmDev$Debugger$viewBranch = F6(
	function (model, depth, path, opener, closer, children) {
		var isCollapsed = A3($author$project$ElmDev$Debugger$branchIsCollapsed, model, depth, path);
		var inlineOpener = A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					A2($author$project$ElmDev$Debugger$ToggleCollapse, path, isCollapsed)),
					A2($elm$html$Html$Attributes$style, 'background', 'transparent'),
					A2($elm$html$Html$Attributes$style, 'border', '0'),
					A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
					A2($elm$html$Html$Attributes$style, 'font', 'inherit'),
					A2($elm$html$Html$Attributes$style, 'font-weight', '800'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
					A2($elm$html$Html$Attributes$style, 'padding', '1px 0')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					isCollapsed ? ('+ ' + (opener + (' ... ' + closer))) : ('- ' + opener))
				]));
		var childNodes = isCollapsed ? _List_Nil : children(_Utils_Tuple0);
		if (isCollapsed) {
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'margin', '2px 0')
					]),
				_List_fromArray(
					[inlineOpener]));
		} else {
			if (!childNodes.b) {
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'margin', '2px 0')
						]),
					_List_fromArray(
						[
							inlineOpener,
							$elm$html$Html$text(' ' + closer)
						]));
			} else {
				var first = childNodes.a;
				var rest = childNodes.b;
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'margin', '2px 0')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'display', 'grid'),
									A2($elm$html$Html$Attributes$style, 'grid-template-columns', 'max-content 1fr'),
									A2($elm$html$Html$Attributes$style, 'column-gap', '1ch'),
									A2($elm$html$Html$Attributes$style, 'align-items', 'start')
								]),
							_List_fromArray(
								[inlineOpener, first])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'border-left', '1px solid rgba(246,164,0,.24)'),
									A2($elm$html$Html$Attributes$style, 'margin-left', '0'),
									A2($elm$html$Html$Attributes$style, 'padding-left', '2ch')
								]),
							rest),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
									A2($elm$html$Html$Attributes$style, 'font-weight', '800'),
									A2($elm$html$Html$Attributes$style, 'padding-left', '2ch')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(closer)
								]))
						]));
			}
		}
	});
var $author$project$ElmDev$Debugger$viewCompactBranch = F6(
	function (model, depth, path, collapsedLabel, openLabel, children) {
		var isCollapsed = A3($author$project$ElmDev$Debugger$branchIsCollapsed, model, depth, path);
		var inlineOpener = A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					A2($author$project$ElmDev$Debugger$ToggleCollapse, path, isCollapsed)),
					A2($elm$html$Html$Attributes$style, 'background', 'transparent'),
					A2($elm$html$Html$Attributes$style, 'border', '0'),
					A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
					A2($elm$html$Html$Attributes$style, 'font', 'inherit'),
					A2($elm$html$Html$Attributes$style, 'font-weight', '800'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
					A2($elm$html$Html$Attributes$style, 'padding', '1px 0')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					isCollapsed ? ('+ ' + collapsedLabel) : ('- ' + openLabel))
				]));
		var childNodes = isCollapsed ? _List_Nil : children(_Utils_Tuple0);
		if (isCollapsed) {
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'margin', '2px 0')
					]),
				_List_fromArray(
					[inlineOpener]));
		} else {
			if (!childNodes.b) {
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'margin', '2px 0')
						]),
					_List_fromArray(
						[inlineOpener]));
			} else {
				var first = childNodes.a;
				var rest = childNodes.b;
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'margin', '2px 0')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'display', 'grid'),
									A2($elm$html$Html$Attributes$style, 'grid-template-columns', 'max-content 1fr'),
									A2($elm$html$Html$Attributes$style, 'column-gap', '1ch'),
									A2($elm$html$Html$Attributes$style, 'align-items', 'start')
								]),
							_List_fromArray(
								[inlineOpener, first])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'border-left', '1px solid rgba(246,164,0,.24)'),
									A2($elm$html$Html$Attributes$style, 'margin-left', '0'),
									A2($elm$html$Html$Attributes$style, 'padding-left', '2ch')
								]),
							rest)
						]));
			}
		}
	});
var $author$project$ElmDev$Debugger$viewInlineSingleArgConstructor = F2(
	function (maybeName, args) {
		if (args.b && (!args.b.b)) {
			var arg = args.a;
			return A2(
				$elm$core$Maybe$map,
				function (inlineArg) {
					return A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
										A2($elm$html$Html$Attributes$style, 'font-weight', '800')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										A2($elm$core$Maybe$withDefault, 'Tuple', maybeName))
									])),
								$elm$html$Html$text(' '),
								inlineArg
							]));
				},
				$author$project$ElmDev$Debugger$viewInlineDebugValue(arg));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$ElmDev$Debugger$viewConstructor = F5(
	function (model, depth, path, maybeName, args) {
		var name = A2($elm$core$Maybe$withDefault, 'Tuple', maybeName);
		if (!args.b) {
			return $author$project$ElmDev$Debugger$viewAtom(name);
		} else {
			var summary = name + (' ' + A2(
				$elm$core$String$join,
				' ',
				A2(
					$elm$core$List$map,
					function (_v12) {
						return '(...)';
					},
					args)));
			return A6(
				$author$project$ElmDev$Debugger$viewCompactBranch,
				model,
				depth,
				path,
				summary,
				name,
				function (_v11) {
					return A2(
						$elm$core$List$indexedMap,
						A3($author$project$ElmDev$Debugger$viewConstructorArg, model, depth + 1, path),
						args);
				});
		}
	});
var $author$project$ElmDev$Debugger$viewConstructorArg = F5(
	function (model, depth, parent, index, value_) {
		var path = parent + ('.' + $elm$core$String$fromInt(index));
		var _v9 = $author$project$ElmDev$Debugger$viewInlineDebugValue(value_);
		if (_v9.$ === 'Just') {
			var inlineValue = _v9.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'padding', '2px 0')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('('),
						inlineValue,
						$elm$html$Html$text(')')
					]));
		} else {
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'padding', '2px 0')
					]),
				_List_fromArray(
					[
						A4($author$project$ElmDev$Debugger$viewDebugValue, model, depth, path, value_)
					]));
		}
	});
var $author$project$ElmDev$Debugger$viewDebugValue = F4(
	function (model, depth, path, value_) {
		switch (value_.$) {
			case 'BoolValue':
				var bool = value_.a;
				return $author$project$ElmDev$Debugger$viewAtom(
					bool ? 'True' : 'False');
			case 'NumberValue':
				var number = value_.a;
				return $author$project$ElmDev$Debugger$viewAtom(number);
			case 'StringValue':
				var string = value_.a;
				return $author$project$ElmDev$Debugger$viewString(string);
			case 'CharValue':
				var _char = value_.a;
				return $author$project$ElmDev$Debugger$viewAtom('\'' + (_char + '\''));
			case 'OpaqueValue':
				var label = value_.a;
				return $author$project$ElmDev$Debugger$viewAtom(label);
			case 'RecordValue':
				var fields = value_.a;
				return A4($author$project$ElmDev$Debugger$viewRecord, model, depth, path, fields);
			case 'ConstructorValue':
				var maybeName = value_.a;
				var args = value_.b;
				var _v8 = A2($author$project$ElmDev$Debugger$viewInlineSingleArgConstructor, maybeName, args);
				if (_v8.$ === 'Just') {
					var inlineConstructor = _v8.a;
					return inlineConstructor;
				} else {
					return A5($author$project$ElmDev$Debugger$viewConstructor, model, depth, path, maybeName, args);
				}
			case 'ListValue':
				var items = value_.a;
				return A6($author$project$ElmDev$Debugger$viewSequence, model, depth, path, '[', ']', items);
			case 'ArrayValue':
				var items = value_.a;
				return A6($author$project$ElmDev$Debugger$viewSequence, model, depth, path, 'Array [', ']', items);
			case 'SetValue':
				var items = value_.a;
				return $elm$core$List$isEmpty(items) ? $author$project$ElmDev$Debugger$viewAtom('Set {}') : A6($author$project$ElmDev$Debugger$viewSequence, model, depth, path, 'Set {', '}', items);
			default:
				var entries = value_.a;
				return A4($author$project$ElmDev$Debugger$viewDict, model, depth, path, entries);
		}
	});
var $author$project$ElmDev$Debugger$viewDict = F4(
	function (model, depth, path, entries) {
		return A6(
			$author$project$ElmDev$Debugger$viewBranch,
			model,
			depth,
			path,
			'Dict {',
			'}',
			function (_v6) {
				return A2(
					$elm$core$List$indexedMap,
					A3($author$project$ElmDev$Debugger$viewDictEntry, model, depth + 1, path),
					entries);
			});
	});
var $author$project$ElmDev$Debugger$viewDictEntry = F5(
	function (model, depth, parent, index, entry) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'grid'),
					A2($elm$html$Html$Attributes$style, 'grid-template-columns', 'max-content 24px 1fr'),
					A2($elm$html$Html$Attributes$style, 'gap', '8px'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'start'),
					A2($elm$html$Html$Attributes$style, 'padding', '2px 0')
				]),
			_List_fromArray(
				[
					A4(
					$author$project$ElmDev$Debugger$viewDebugValue,
					model,
					depth,
					parent + ('.' + ($elm$core$String$fromInt(index) + '.key')),
					entry.key),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('=>')
						])),
					A4(
					$author$project$ElmDev$Debugger$viewDebugValue,
					model,
					depth,
					parent + ('.' + ($elm$core$String$fromInt(index) + '.value')),
					entry.value)
				]));
	});
var $author$project$ElmDev$Debugger$viewField = F5(
	function (model, depth, parent, index, field) {
		var _v5 = field.value;
		if (_v5.$ === 'RecordValue') {
			var fields = _v5.a;
			return ($elm$core$List$length(fields) > 1) ? A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'padding', '2px 0')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'color', '#f6a400')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								$author$project$ElmDev$Debugger$fieldPrefix(index) + (field.name + ':'))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'padding-left', '2ch')
							]),
						_List_fromArray(
							[
								A4($author$project$ElmDev$Debugger$viewDebugValue, model, depth, parent + ('.' + field.name), field.value)
							]))
					])) : A5($author$project$ElmDev$Debugger$viewInlineFieldRow, model, depth, parent, index, field);
		} else {
			return $author$project$ElmDev$Debugger$isMultilineValue(field.value) ? A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'padding', '2px 0')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'color', '#f6a400')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								$author$project$ElmDev$Debugger$fieldPrefix(index) + (field.name + ':'))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'padding-left', '2ch')
							]),
						_List_fromArray(
							[
								A4($author$project$ElmDev$Debugger$viewDebugValue, model, depth, parent + ('.' + field.name), field.value)
							]))
					])) : A5($author$project$ElmDev$Debugger$viewInlineFieldRow, model, depth, parent, index, field);
		}
	});
var $author$project$ElmDev$Debugger$viewIndexed = F5(
	function (model, depth, parent, index, value_) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'grid'),
					A2($elm$html$Html$Attributes$style, 'grid-template-columns', 'max-content 1fr'),
					A2($elm$html$Html$Attributes$style, 'column-gap', '8px'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'start'),
					A2($elm$html$Html$Attributes$style, 'padding', '2px 0')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(index) + ':')
						])),
					A4(
					$author$project$ElmDev$Debugger$viewDebugValue,
					model,
					depth,
					parent + ('.' + $elm$core$String$fromInt(index)),
					value_)
				]));
	});
var $author$project$ElmDev$Debugger$viewInlineFieldRow = F5(
	function (model, depth, parent, index, field) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'grid'),
					A2($elm$html$Html$Attributes$style, 'grid-template-columns', 'max-content 1fr'),
					A2($elm$html$Html$Attributes$style, 'column-gap', '1ch'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'start'),
					A2($elm$html$Html$Attributes$style, 'padding', '2px 0')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'color', '#f6a400')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$ElmDev$Debugger$fieldPrefix(index) + (field.name + ':'))
						])),
					A4($author$project$ElmDev$Debugger$viewDebugValue, model, depth, parent + ('.' + field.name), field.value)
				]));
	});
var $author$project$ElmDev$Debugger$viewRecord = F4(
	function (model, depth, path, fields) {
		if (fields.b && (!fields.b.b)) {
			var field = fields.a;
			var _v2 = $author$project$ElmDev$Debugger$viewInlineField(field);
			if (_v2.$ === 'Just') {
				var inlineField = _v2.a;
				return A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('{ '),
							inlineField,
							$elm$html$Html$text(' }')
						]));
			} else {
				return A6(
					$author$project$ElmDev$Debugger$viewBranch,
					model,
					depth,
					path,
					'{',
					'}',
					function (_v3) {
						return A2(
							$elm$core$List$indexedMap,
							A3($author$project$ElmDev$Debugger$viewField, model, depth + 1, path),
							fields);
					});
			}
		} else {
			return A6(
				$author$project$ElmDev$Debugger$viewBranch,
				model,
				depth,
				path,
				'{',
				'}',
				function (_v4) {
					return A2(
						$elm$core$List$indexedMap,
						A3($author$project$ElmDev$Debugger$viewField, model, depth + 1, path),
						fields);
				});
		}
	});
var $author$project$ElmDev$Debugger$viewSequence = F6(
	function (model, depth, path, opener, closer, items) {
		return A6(
			$author$project$ElmDev$Debugger$viewBranch,
			model,
			depth,
			path,
			opener,
			closer,
			function (_v0) {
				return A2(
					$elm$core$List$indexedMap,
					A3($author$project$ElmDev$Debugger$viewIndexed, model, depth + 1, path),
					items);
			});
	});
var $author$project$ElmDev$Debugger$viewLogEntry = F2(
	function (model, logEntry) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.24)'),
					A2($elm$html$Html$Attributes$style, 'border-radius', '12px'),
					A2($elm$html$Html$Attributes$style, 'background', 'rgba(13,10,5,.52)'),
					A2($elm$html$Html$Attributes$style, 'overflow', 'hidden')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'padding', '10px 12px'),
							A2($elm$html$Html$Attributes$style, 'display', 'grid'),
							A2($elm$html$Html$Attributes$style, 'grid-template-columns', '1fr max-content'),
							A2($elm$html$Html$Attributes$style, 'gap', '14px'),
							A2($elm$html$Html$Attributes$style, 'align-items', 'baseline'),
							A2($elm$html$Html$Attributes$style, 'background', 'rgba(246,164,0,.08)')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$style, 'font-size', '15px'),
											A2($elm$html$Html$Attributes$style, 'font-weight', '900'),
											A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
											A2($elm$html$Html$Attributes$style, 'overflow-wrap', 'anywhere')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(logEntry.label)
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$style, 'color', '#9f7a31'),
											A2($elm$html$Html$Attributes$style, 'font-size', '12px'),
											A2($elm$html$Html$Attributes$style, 'margin-top', '3px'),
											A2($elm$html$Html$Attributes$style, 'overflow-wrap', 'anywhere')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											$author$project$ElmDev$Debugger$logSourceKey(logEntry))
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'color', '#f6a400'),
									A2($elm$html$Html$Attributes$style, 'font-weight', '900')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									'#' + $elm$core$String$fromInt(logEntry.logId))
								]))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'padding', '12px'),
							A2($elm$html$Html$Attributes$style, 'background', 'rgba(8,9,10,.66)')
						]),
					_List_fromArray(
						[
							A4(
							$author$project$ElmDev$Debugger$viewDebugValue,
							model,
							0,
							'log-' + $elm$core$String$fromInt(logEntry.logId),
							logEntry.value)
						]))
				]));
	});
var $author$project$ElmDev$Debugger$countLogsForSource = F2(
	function (source, logs) {
		return $elm$core$List$length(
			A2(
				$elm$core$List$filter,
				function (logEntry) {
					return _Utils_eq(
						$author$project$ElmDev$Debugger$logSourceKey(logEntry),
						source);
				},
				logs));
	});
var $author$project$ElmDev$Debugger$SelectLogSource = function (a) {
	return {$: 'SelectLogSource', a: a};
};
var $author$project$ElmDev$Debugger$logFilterButton = F3(
	function (selectedSource, source, label) {
		var selected = _Utils_eq(selectedSource, source);
		return A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$ElmDev$Debugger$SelectLogSource(source)),
					A2(
					$elm$html$Html$Attributes$style,
					'background',
					selected ? 'rgba(246,164,0,.24)' : 'rgba(246,164,0,.06)'),
					A2(
					$elm$html$Html$Attributes$style,
					'border',
					selected ? '1px solid rgba(246,164,0,.75)' : '1px solid rgba(246,164,0,.24)'),
					A2($elm$html$Html$Attributes$style, 'border-radius', '999px'),
					A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
					A2($elm$html$Html$Attributes$style, 'font', 'inherit'),
					A2($elm$html$Html$Attributes$style, 'font-weight', '800'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
					A2($elm$html$Html$Attributes$style, 'padding', '5px 9px'),
					$elm$html$Html$Attributes$title(label)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(label)
				]));
	});
var $author$project$ElmDev$Debugger$shortSourceLabel = function (source) {
	var _v0 = $elm$core$List$reverse(
		A2($elm$core$String$split, '/', source));
	if (_v0.b) {
		var file = _v0.a;
		return file;
	} else {
		return source;
	}
};
var $author$project$ElmDev$Debugger$viewLogFilters = F3(
	function (selectedSource, logs, sources) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'flex-wrap', 'wrap'),
					A2($elm$html$Html$Attributes$style, 'gap', '8px'),
					A2($elm$html$Html$Attributes$style, 'margin-bottom', '18px')
				]),
			A2(
				$elm$core$List$cons,
				A3(
					$author$project$ElmDev$Debugger$logFilterButton,
					selectedSource,
					$elm$core$Maybe$Nothing,
					'All (' + ($elm$core$String$fromInt(
						$elm$core$List$length(logs)) + ')')),
				A2(
					$elm$core$List$map,
					function (source) {
						return A3(
							$author$project$ElmDev$Debugger$logFilterButton,
							selectedSource,
							$elm$core$Maybe$Just(source),
							$author$project$ElmDev$Debugger$shortSourceLabel(source) + (' (' + ($elm$core$String$fromInt(
								A2($author$project$ElmDev$Debugger$countLogsForSource, source, logs)) + ')')));
					},
					sources)));
	});
var $author$project$ElmDev$Debugger$viewLogs = function (model) {
	var visibleLogs = function () {
		var _v0 = model.selectedLogSource;
		if (_v0.$ === 'Nothing') {
			return model.debugLogs;
		} else {
			var selectedSource = _v0.a;
			return A2(
				$elm$core$List$filter,
				function (logEntry) {
					return _Utils_eq(
						$author$project$ElmDev$Debugger$logSourceKey(logEntry),
						selectedSource);
				},
				model.debugLogs);
		}
	}();
	var sources = $author$project$ElmDev$Debugger$logSources(model.debugLogs);
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'overflow', 'auto'),
				A2($elm$html$Html$Attributes$style, 'flex', '1'),
				A2($elm$html$Html$Attributes$style, 'min-width', '0'),
				A2($elm$html$Html$Attributes$style, 'padding', '22px')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'flex'),
						A2($elm$html$Html$Attributes$style, 'align-items', 'baseline'),
						A2($elm$html$Html$Attributes$style, 'justify-content', 'space-between'),
						A2($elm$html$Html$Attributes$style, 'gap', '20px'),
						A2($elm$html$Html$Attributes$style, 'margin-bottom', '18px')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'font-size', '11px'),
										A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase'),
										A2($elm$html$Html$Attributes$style, 'letter-spacing', '0.16em'),
										A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('debug history')
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'font-size', '24px'),
										A2($elm$html$Html$Attributes$style, 'font-weight', '900'),
										A2($elm$html$Html$Attributes$style, 'color', '#ffd77a')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Debug.log')
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'color', '#9f7a31'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'right')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Logs are grouped by source call site when location metadata is available.')
							]))
					])),
				A3($author$project$ElmDev$Debugger$viewLogFilters, model.selectedLogSource, model.debugLogs, sources),
				$elm$core$List$isEmpty(visibleLogs) ? A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'padding', '24px'),
						A2($elm$html$Html$Attributes$style, 'border', '1px dashed rgba(246,164,0,.35)'),
						A2($elm$html$Html$Attributes$style, 'border-radius', '12px'),
						A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('No Debug.log entries observed yet.')
					])) : A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'grid'),
						A2($elm$html$Html$Attributes$style, 'gap', '10px')
					]),
				A2(
					$elm$core$List$map,
					$author$project$ElmDev$Debugger$viewLogEntry(model),
					$elm$core$List$reverse(visibleLogs)))
			]));
};
var $author$project$ElmDev$Debugger$LazyView = {$: 'LazyView'};
var $author$project$ElmDev$Debugger$LogsView = {$: 'LogsView'};
var $author$project$ElmDev$Debugger$SelectView = function (a) {
	return {$: 'SelectView', a: a};
};
var $author$project$ElmDev$Debugger$viewModeButton = F3(
	function (current, target, label) {
		var selected = _Utils_eq(current, target);
		return A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$ElmDev$Debugger$SelectView(target)),
					A2(
					$elm$html$Html$Attributes$style,
					'background',
					selected ? 'rgba(246,164,0,.24)' : 'rgba(246,164,0,.06)'),
					A2(
					$elm$html$Html$Attributes$style,
					'border',
					selected ? '1px solid rgba(246,164,0,.75)' : '1px solid rgba(246,164,0,.24)'),
					A2($elm$html$Html$Attributes$style, 'border-radius', '8px'),
					A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
					A2($elm$html$Html$Attributes$style, 'font', 'inherit'),
					A2($elm$html$Html$Attributes$style, 'font-weight', '800'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
					A2($elm$html$Html$Attributes$style, 'padding', '7px 8px')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(label)
				]));
	});
var $author$project$ElmDev$Debugger$viewModeTabs = F2(
	function (viewMode, lazyStats) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'grid'),
					A2($elm$html$Html$Attributes$style, 'grid-template-columns', '1fr 1fr 1fr'),
					A2($elm$html$Html$Attributes$style, 'gap', '8px'),
					A2($elm$html$Html$Attributes$style, 'padding', '10px 12px'),
					A2($elm$html$Html$Attributes$style, 'border-bottom', '1px solid rgba(246, 164, 0, .28)')
				]),
			_List_fromArray(
				[
					A3($author$project$ElmDev$Debugger$viewModeButton, viewMode, $author$project$ElmDev$Debugger$TimelineView, 'Timeline'),
					A3(
					$author$project$ElmDev$Debugger$viewModeButton,
					viewMode,
					$author$project$ElmDev$Debugger$LazyView,
					'Lazy (' + ($elm$core$String$fromInt(
						$elm$core$List$length(lazyStats)) + ')')),
					A3($author$project$ElmDev$Debugger$viewModeButton, viewMode, $author$project$ElmDev$Debugger$LogsView, 'Logs')
				]));
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $author$project$ElmDev$Debugger$eventId = function (event) {
	switch (event.$) {
		case 'Init':
			var initEvent = event.a;
			return initEvent.id;
		case 'Update':
			var updateEvent = event.a;
			return updateEvent.id;
		case 'Frame':
			var frameEvent = event.a;
			return frameEvent.id;
		default:
			return -1;
	}
};
var $author$project$ElmDev$Debugger$eventById = F2(
	function (events, id) {
		eventById:
		while (true) {
			if (!events.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var event = events.a;
				var rest = events.b;
				if (_Utils_eq(
					$author$project$ElmDev$Debugger$eventId(event),
					id)) {
					return $elm$core$Maybe$Just(event);
				} else {
					var $temp$events = rest,
						$temp$id = id;
					events = $temp$events;
					id = $temp$id;
					continue eventById;
				}
			}
		}
	});
var $author$project$ElmDev$Debugger$ToggleMessagePayload = function (a) {
	return {$: 'ToggleMessagePayload', a: a};
};
var $author$project$ElmDev$Debugger$viewDetailHeader = F4(
	function (label, maybeDuration, maybePayloadId, payloadOpen) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'padding', '18px 22px'),
					A2($elm$html$Html$Attributes$style, 'border-bottom', '1px solid rgba(246,164,0,.34)'),
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'baseline'),
					A2($elm$html$Html$Attributes$style, 'gap', '18px'),
					A2($elm$html$Html$Attributes$style, 'justify-content', 'space-between'),
					A2($elm$html$Html$Attributes$style, 'background', 'rgba(246,164,0,.08)')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'font-size', '11px'),
									A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase'),
									A2($elm$html$Html$Attributes$style, 'letter-spacing', '0.16em'),
									A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('message')
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'font-size', '22px'),
									A2($elm$html$Html$Attributes$style, 'font-weight', '800'),
									A2($elm$html$Html$Attributes$style, 'color', '#ffd77a'),
									A2($elm$html$Html$Attributes$style, 'overflow-wrap', 'anywhere'),
									A2($elm$html$Html$Attributes$style, 'display', 'flex'),
									A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
									A2($elm$html$Html$Attributes$style, 'gap', '10px')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(label),
									function () {
									if (maybePayloadId.$ === 'Just') {
										var id = maybePayloadId.a;
										return A2(
											$elm$html$Html$button,
											_List_fromArray(
												[
													$elm$html$Html$Events$onClick(
													$author$project$ElmDev$Debugger$ToggleMessagePayload(id)),
													A2($elm$html$Html$Attributes$style, 'background', 'transparent'),
													A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.65)'),
													A2($elm$html$Html$Attributes$style, 'border-radius', '4px'),
													A2($elm$html$Html$Attributes$style, 'color', '#f6a400'),
													A2($elm$html$Html$Attributes$style, 'font', 'inherit'),
													A2($elm$html$Html$Attributes$style, 'font-weight', '900'),
													A2($elm$html$Html$Attributes$style, 'line-height', '1'),
													A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
													A2($elm$html$Html$Attributes$style, 'padding', '2px 7px'),
													$elm$html$Html$Attributes$title('Toggle message payload')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(
													payloadOpen ? '-' : '+')
												]));
									} else {
										return $elm$html$Html$text('');
									}
								}()
								]))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'text-align', 'right'),
							A2($elm$html$Html$Attributes$style, 'white-space', 'nowrap')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'font-size', '11px'),
									A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase'),
									A2($elm$html$Html$Attributes$style, 'letter-spacing', '0.16em'),
									A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('update duration')
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'font-size', '18px'),
									A2($elm$html$Html$Attributes$style, 'font-weight', '800'),
									A2($elm$html$Html$Attributes$style, 'color', '#f6a400')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									A2(
										$elm$core$Maybe$withDefault,
										'-',
										A2($elm$core$Maybe$map, $author$project$ElmDev$Debugger$formatMs, maybeDuration)))
								]))
						]))
				]));
	});
var $author$project$ElmDev$Debugger$fieldByName = F2(
	function (name, fields) {
		fieldByName:
		while (true) {
			if (!fields.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var field = fields.a;
				var rest = fields.b;
				if (_Utils_eq(field.name, name)) {
					return $elm$core$Maybe$Just(field);
				} else {
					var $temp$name = name,
						$temp$fields = rest;
					name = $temp$name;
					fields = $temp$fields;
					continue fieldByName;
				}
			}
		}
	});
var $author$project$ElmDev$Debugger$listAt = F2(
	function (index, list) {
		listAt:
		while (true) {
			if (index < 0) {
				return $elm$core$Maybe$Nothing;
			} else {
				var _v0 = _Utils_Tuple2(index, list);
				if (_v0.b.b) {
					if (!_v0.a) {
						var _v1 = _v0.b;
						var item = _v1.a;
						return $elm$core$Maybe$Just(item);
					} else {
						var _v2 = _v0.b;
						var rest = _v2.b;
						var $temp$index = index - 1,
							$temp$list = rest;
						index = $temp$index;
						list = $temp$list;
						continue listAt;
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $author$project$ElmDev$Debugger$viewDeltaValue = F4(
	function (model, path, label, value_) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.18)'),
					A2($elm$html$Html$Attributes$style, 'border-radius', '8px'),
					A2($elm$html$Html$Attributes$style, 'padding', '9px'),
					A2($elm$html$Html$Attributes$style, 'background', 'rgba(0,0,0,.22)'),
					A2($elm$html$Html$Attributes$style, 'min-width', '0')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'font-size', '10px'),
							A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase'),
							A2($elm$html$Html$Attributes$style, 'letter-spacing', '0.14em'),
							A2($elm$html$Html$Attributes$style, 'color', '#9f7a31'),
							A2($elm$html$Html$Attributes$style, 'margin-bottom', '6px')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A4($author$project$ElmDev$Debugger$viewDebugValue, model, 0, path, value_)
				]));
	});
var $author$project$ElmDev$Debugger$viewChangedValue = F4(
	function (model, path, before, after) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'grid'),
					A2($elm$html$Html$Attributes$style, 'grid-template-columns', 'minmax(0, 1fr) max-content minmax(0, 1fr)'),
					A2($elm$html$Html$Attributes$style, 'gap', '10px'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'stretch'),
					A2($elm$html$Html$Attributes$style, 'padding', '10px')
				]),
			_List_fromArray(
				[
					A4($author$project$ElmDev$Debugger$viewDeltaValue, model, path + '.before', 'before', before),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'display', 'flex'),
							A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
							A2($elm$html$Html$Attributes$style, 'color', '#f6a400'),
							A2($elm$html$Html$Attributes$style, 'font-weight', '900')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('->')
						])),
					A4($author$project$ElmDev$Debugger$viewDeltaValue, model, path + '.after', 'after', after)
				]));
	});
var $author$project$ElmDev$Debugger$changedIndexedItems = F4(
	function (model, path, beforeItems, afterItems) {
		return A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			A2(
				$elm$core$List$indexedMap,
				A3($author$project$ElmDev$Debugger$viewChangedIndexed, model, path, beforeItems),
				afterItems));
	});
var $author$project$ElmDev$Debugger$changedRecordFields = F4(
	function (model, path, beforeFields, afterFields) {
		return A2(
			$elm$core$List$filterMap,
			function (afterField) {
				var _v6 = A2($author$project$ElmDev$Debugger$fieldByName, afterField.name, beforeFields);
				if (_v6.$ === 'Just') {
					var beforeField = _v6.a;
					return _Utils_eq(beforeField.value, afterField.value) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
						A5($author$project$ElmDev$Debugger$viewChangedField, model, path + ('.' + afterField.name), afterField.name, beforeField.value, afterField.value));
				} else {
					return $elm$core$Maybe$Just(
						A5(
							$author$project$ElmDev$Debugger$viewChangedField,
							model,
							path + ('.' + afterField.name),
							afterField.name,
							$author$project$ElmDev$Debugger$OpaqueValue('<missing>'),
							afterField.value));
				}
			},
			afterFields);
	});
var $author$project$ElmDev$Debugger$deltaChildren = F4(
	function (model, path, before, after) {
		var _v3 = _Utils_Tuple2(before, after);
		_v3$4:
		while (true) {
			switch (_v3.a.$) {
				case 'RecordValue':
					if (_v3.b.$ === 'RecordValue') {
						var beforeFields = _v3.a.a;
						var afterFields = _v3.b.a;
						return A4($author$project$ElmDev$Debugger$changedRecordFields, model, path, beforeFields, afterFields);
					} else {
						break _v3$4;
					}
				case 'ConstructorValue':
					if (_v3.b.$ === 'ConstructorValue') {
						var _v4 = _v3.a;
						var beforeName = _v4.a;
						var beforeArgs = _v4.b;
						var _v5 = _v3.b;
						var afterName = _v5.a;
						var afterArgs = _v5.b;
						return (_Utils_eq(beforeName, afterName) && _Utils_eq(
							$elm$core$List$length(beforeArgs),
							$elm$core$List$length(afterArgs))) ? A2(
							$elm$core$List$filterMap,
							$elm$core$Basics$identity,
							A2(
								$elm$core$List$indexedMap,
								A3($author$project$ElmDev$Debugger$viewChangedConstructorArg, model, path, beforeArgs),
								afterArgs)) : _List_Nil;
					} else {
						break _v3$4;
					}
				case 'ListValue':
					if (_v3.b.$ === 'ListValue') {
						var beforeItems = _v3.a.a;
						var afterItems = _v3.b.a;
						return A4($author$project$ElmDev$Debugger$changedIndexedItems, model, path, beforeItems, afterItems);
					} else {
						break _v3$4;
					}
				case 'ArrayValue':
					if (_v3.b.$ === 'ArrayValue') {
						var beforeItems = _v3.a.a;
						var afterItems = _v3.b.a;
						return A4($author$project$ElmDev$Debugger$changedIndexedItems, model, path, beforeItems, afterItems);
					} else {
						break _v3$4;
					}
				default:
					break _v3$4;
			}
		}
		return _List_Nil;
	});
var $author$project$ElmDev$Debugger$viewChangedConstructorArg = F5(
	function (model, path, beforeArgs, index, after) {
		var _v2 = A2($author$project$ElmDev$Debugger$listAt, index, beforeArgs);
		if (_v2.$ === 'Just') {
			var before = _v2.a;
			return _Utils_eq(before, after) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
				A5(
					$author$project$ElmDev$Debugger$viewChangedField,
					model,
					path + ('.' + $elm$core$String$fromInt(index)),
					'arg ' + $elm$core$String$fromInt(index),
					before,
					after));
		} else {
			return $elm$core$Maybe$Just(
				A5(
					$author$project$ElmDev$Debugger$viewChangedField,
					model,
					path + ('.' + $elm$core$String$fromInt(index)),
					'arg ' + $elm$core$String$fromInt(index),
					$author$project$ElmDev$Debugger$OpaqueValue('<missing>'),
					after));
		}
	});
var $author$project$ElmDev$Debugger$viewChangedField = F5(
	function (model, path, label, before, after) {
		var _v1 = A4($author$project$ElmDev$Debugger$deltaChildren, model, path, before, after);
		if (!_v1.b) {
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.24)'),
						A2($elm$html$Html$Attributes$style, 'border-radius', '10px'),
						A2($elm$html$Html$Attributes$style, 'background', 'rgba(8,9,10,.48)'),
						A2($elm$html$Html$Attributes$style, 'overflow', 'hidden')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'padding', '8px 11px'),
								A2($elm$html$Html$Attributes$style, 'background', 'rgba(246,164,0,.1)'),
								A2($elm$html$Html$Attributes$style, 'color', '#f6a400'),
								A2($elm$html$Html$Attributes$style, 'font-weight', '900')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(label)
							])),
						A4($author$project$ElmDev$Debugger$viewChangedValue, model, path, before, after)
					]));
		} else {
			var children = _v1;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.24)'),
						A2($elm$html$Html$Attributes$style, 'border-radius', '10px'),
						A2($elm$html$Html$Attributes$style, 'background', 'rgba(8,9,10,.35)'),
						A2($elm$html$Html$Attributes$style, 'overflow', 'hidden')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'padding', '8px 11px'),
								A2($elm$html$Html$Attributes$style, 'background', 'rgba(246,164,0,.1)'),
								A2($elm$html$Html$Attributes$style, 'color', '#f6a400'),
								A2($elm$html$Html$Attributes$style, 'font-weight', '900')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(label)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'grid'),
								A2($elm$html$Html$Attributes$style, 'gap', '8px'),
								A2($elm$html$Html$Attributes$style, 'padding', '10px')
							]),
						children)
					]));
		}
	});
var $author$project$ElmDev$Debugger$viewChangedIndexed = F5(
	function (model, path, beforeItems, index, after) {
		var _v0 = A2($author$project$ElmDev$Debugger$listAt, index, beforeItems);
		if (_v0.$ === 'Just') {
			var before = _v0.a;
			return _Utils_eq(before, after) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
				A5(
					$author$project$ElmDev$Debugger$viewChangedField,
					model,
					path + ('.' + $elm$core$String$fromInt(index)),
					$elm$core$String$fromInt(index),
					before,
					after));
		} else {
			return $elm$core$Maybe$Just(
				A5(
					$author$project$ElmDev$Debugger$viewChangedField,
					model,
					path + ('.' + $elm$core$String$fromInt(index)),
					$elm$core$String$fromInt(index),
					$author$project$ElmDev$Debugger$OpaqueValue('<missing>'),
					after));
		}
	});
var $author$project$ElmDev$Debugger$viewDebugDelta = F4(
	function (model, path, before, after) {
		if (_Utils_eq(before, after)) {
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'padding', '18px'),
						A2($elm$html$Html$Attributes$style, 'border', '1px dashed rgba(246,164,0,.28)'),
						A2($elm$html$Html$Attributes$style, 'border-radius', '10px'),
						A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('No model changes for this message.')
					]));
		} else {
			var _v0 = A4($author$project$ElmDev$Debugger$deltaChildren, model, path, before, after);
			if (!_v0.b) {
				return A4($author$project$ElmDev$Debugger$viewChangedValue, model, path, before, after);
			} else {
				var children = _v0;
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'display', 'grid'),
							A2($elm$html$Html$Attributes$style, 'gap', '10px')
						]),
					children);
			}
		}
	});
var $author$project$ElmDev$Debugger$ModelAfter = {$: 'ModelAfter'};
var $author$project$ElmDev$Debugger$ModelBefore = {$: 'ModelBefore'};
var $author$project$ElmDev$Debugger$SelectModelView = function (a) {
	return {$: 'SelectModelView', a: a};
};
var $author$project$ElmDev$Debugger$viewModelToggleButton = F3(
	function (current, target, label) {
		var selected = _Utils_eq(current, target);
		return A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$ElmDev$Debugger$SelectModelView(target)),
					A2($elm$html$Html$Attributes$style, 'border', '0'),
					A2($elm$html$Html$Attributes$style, 'border-radius', '999px'),
					A2(
					$elm$html$Html$Attributes$style,
					'background',
					selected ? '#f6a400' : 'transparent'),
					A2(
					$elm$html$Html$Attributes$style,
					'color',
					selected ? '#140c00' : '#ffd77a'),
					A2($elm$html$Html$Attributes$style, 'font', 'inherit'),
					A2($elm$html$Html$Attributes$style, 'font-weight', '900'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
					A2($elm$html$Html$Attributes$style, 'padding', '5px 11px')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(label)
				]));
	});
var $author$project$ElmDev$Debugger$viewModelToggle = function (current) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'display', 'inline-grid'),
				A2($elm$html$Html$Attributes$style, 'grid-template-columns', 'repeat(3, max-content)'),
				A2($elm$html$Html$Attributes$style, 'gap', '4px'),
				A2($elm$html$Html$Attributes$style, 'padding', '3px'),
				A2($elm$html$Html$Attributes$style, 'border', '1px solid rgba(246,164,0,.28)'),
				A2($elm$html$Html$Attributes$style, 'border-radius', '999px'),
				A2($elm$html$Html$Attributes$style, 'background', 'rgba(8,9,10,.46)')
			]),
		_List_fromArray(
			[
				A3($author$project$ElmDev$Debugger$viewModelToggleButton, current, $author$project$ElmDev$Debugger$ModelBefore, 'Before'),
				A3($author$project$ElmDev$Debugger$viewModelToggleButton, current, $author$project$ElmDev$Debugger$ModelAfter, 'After'),
				A3($author$project$ElmDev$Debugger$viewModelToggleButton, current, $author$project$ElmDev$Debugger$ModelDelta, 'Delta')
			]));
};
var $author$project$ElmDev$Debugger$viewModelSection = F2(
	function (model, updateEvent) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'padding', '18px 22px'),
					A2($elm$html$Html$Attributes$style, 'border-bottom', '1px solid rgba(246,164,0,.12)')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'display', 'flex'),
							A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
							A2($elm$html$Html$Attributes$style, 'justify-content', 'space-between'),
							A2($elm$html$Html$Attributes$style, 'gap', '16px'),
							A2($elm$html$Html$Attributes$style, 'margin-bottom', '14px')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'font-size', '11px'),
									A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase'),
									A2($elm$html$Html$Attributes$style, 'letter-spacing', '0.16em'),
									A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('model')
								])),
							$author$project$ElmDev$Debugger$viewModelToggle(model.modelView)
						])),
					function () {
					var _v0 = model.modelView;
					switch (_v0.$) {
						case 'ModelBefore':
							return A4($author$project$ElmDev$Debugger$viewDebugValue, model, 0, 'before', updateEvent.modelBefore);
						case 'ModelAfter':
							return A4($author$project$ElmDev$Debugger$viewDebugValue, model, 0, 'after', updateEvent.modelAfter);
						default:
							return A4($author$project$ElmDev$Debugger$viewDebugDelta, model, 'delta', updateEvent.modelBefore, updateEvent.modelAfter);
					}
				}()
				]));
	});
var $author$project$ElmDev$Debugger$viewSection = F3(
	function (model, label, value_) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'padding', '18px 22px'),
					A2($elm$html$Html$Attributes$style, 'border-bottom', '1px solid rgba(246,164,0,.12)')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'font-size', '11px'),
							A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase'),
							A2($elm$html$Html$Attributes$style, 'letter-spacing', '0.16em'),
							A2($elm$html$Html$Attributes$style, 'color', '#9f7a31'),
							A2($elm$html$Html$Attributes$style, 'margin-bottom', '10px')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A4($author$project$ElmDev$Debugger$viewDebugValue, model, 0, label, value_)
				]));
	});
var $author$project$ElmDev$Debugger$viewEventDetails = F2(
	function (model, event) {
		switch (event.$) {
			case 'Init':
				var initEvent = event.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A4($author$project$ElmDev$Debugger$viewDetailHeader, 'init', $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, false),
							A3($author$project$ElmDev$Debugger$viewSection, model, 'model', initEvent.model)
						]));
			case 'Update':
				var updateEvent = event.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A4(
							$author$project$ElmDev$Debugger$viewDetailHeader,
							updateEvent.message,
							$elm$core$Maybe$Just(updateEvent.duration),
							$elm$core$Maybe$Just(updateEvent.id),
							A2($elm$core$Set$member, updateEvent.id, model.messageDrawers)),
							A2($elm$core$Set$member, updateEvent.id, model.messageDrawers) ? A3(
							$author$project$ElmDev$Debugger$viewSection,
							model,
							'message-' + $elm$core$String$fromInt(updateEvent.id),
							updateEvent.rawMessage) : $elm$html$Html$text(''),
							A2($author$project$ElmDev$Debugger$viewModelSection, model, updateEvent)
						]));
			case 'Frame':
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'padding', '24px')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Frame finished')
						]));
			default:
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'padding', '24px')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Unknown debugger event')
						]));
		}
	});
var $author$project$ElmDev$Debugger$viewSelected = function (model) {
	var _v0 = A2(
		$elm$core$Maybe$andThen,
		$author$project$ElmDev$Debugger$eventById(model.events),
		model.selected);
	if (_v0.$ === 'Nothing') {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'padding', '24px'),
					A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Waiting for debugger events...')
				]));
	} else {
		var event = _v0.a;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'overflow', 'auto'),
					A2($elm$html$Html$Attributes$style, 'flex', '1'),
					A2($elm$html$Html$Attributes$style, 'min-width', '0')
				]),
			_List_fromArray(
				[
					A2($author$project$ElmDev$Debugger$viewEventDetails, model, event)
				]));
	}
};
var $author$project$ElmDev$Debugger$Scrub = function (a) {
	return {$: 'Scrub', a: a};
};
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Attributes$max = $elm$html$Html$Attributes$stringProperty('max');
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$html$Html$Attributes$min = $elm$html$Html$Attributes$stringProperty('min');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$ElmDev$Debugger$Select = function (a) {
	return {$: 'Select', a: a};
};
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $author$project$ElmDev$Debugger$eventLabel = function (event) {
	switch (event.$) {
		case 'Init':
			return 'init';
		case 'Update':
			var updateEvent = event.a;
			return updateEvent.message;
		case 'Frame':
			return 'frame';
		default:
			return 'unknown';
	}
};
var $author$project$ElmDev$Debugger$viewEventSummary = F2(
	function (selected, event) {
		if (event.$ === 'Frame') {
			var frameEvent = event.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'padding', '9px 12px'),
						A2($elm$html$Html$Attributes$style, 'color', '#9f7a31'),
						A2($elm$html$Html$Attributes$style, 'font-size', '11px'),
						A2($elm$html$Html$Attributes$style, 'letter-spacing', '0.12em'),
						A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase'),
						A2($elm$html$Html$Attributes$style, 'border-top', '1px solid rgba(246,164,0,.18)'),
						A2($elm$html$Html$Attributes$style, 'border-bottom', '1px solid rgba(246,164,0,.18)'),
						A2($elm$html$Html$Attributes$style, 'display', 'grid'),
						A2($elm$html$Html$Attributes$style, 'grid-template-columns', '1fr max-content'),
						A2($elm$html$Html$Attributes$style, 'column-gap', '16px'),
						A2($elm$html$Html$Attributes$style, 'align-items', 'center')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Animation Frame')
							])),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'text-align', 'right')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								'update ' + ($author$project$ElmDev$Debugger$formatMs(frameEvent.duration) + (' / render ' + $author$project$ElmDev$Debugger$formatMs(frameEvent.renderDuration))))
							]))
					]));
		} else {
			var id = $author$project$ElmDev$Debugger$eventId(event);
			var bg = _Utils_eq(selected, id) ? 'rgba(246,164,0,.22)' : 'transparent';
			return A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick(
						$author$project$ElmDev$Debugger$Select(id)),
						$elm$html$Html$Attributes$disabled(id < 0),
						A2($elm$html$Html$Attributes$style, 'display', 'block'),
						A2($elm$html$Html$Attributes$style, 'width', '100%'),
						A2($elm$html$Html$Attributes$style, 'text-align', 'left'),
						A2($elm$html$Html$Attributes$style, 'font', 'inherit'),
						A2($elm$html$Html$Attributes$style, 'color', 'inherit'),
						A2($elm$html$Html$Attributes$style, 'background', bg),
						A2($elm$html$Html$Attributes$style, 'border', '0'),
						A2(
						$elm$html$Html$Attributes$style,
						'border-left',
						_Utils_eq(selected, id) ? '3px solid #f6a400' : '3px solid transparent'),
						A2($elm$html$Html$Attributes$style, 'padding', '9px 12px'),
						A2($elm$html$Html$Attributes$style, 'cursor', 'pointer')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'color', '#9f7a31')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								$elm$core$String$fromInt(id) + '  ')
							])),
						$elm$html$Html$text(
						$author$project$ElmDev$Debugger$eventLabel(event))
					]));
		}
	});
var $author$project$ElmDev$Debugger$viewTimelineSidebar = function (model) {
	var maxId = A2(
		$elm$core$Maybe$withDefault,
		0,
		$elm$core$List$maximum(
			A2(
				$elm$core$List$map,
				$author$project$ElmDev$Debugger$eventId,
				$author$project$ElmDev$Debugger$selectableEvents(model.events))));
	var selected = A2($elm$core$Maybe$withDefault, maxId, model.selected);
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'width', '340px'),
				A2($elm$html$Html$Attributes$style, 'border-right', '1px solid rgba(246, 164, 0, .45)'),
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				A2($elm$html$Html$Attributes$style, 'flex-direction', 'column'),
				A2($elm$html$Html$Attributes$style, 'background', 'rgba(13, 10, 5, .74)'),
				A2($elm$html$Html$Attributes$style, 'min-height', '0')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'padding', '12px'),
						A2($elm$html$Html$Attributes$style, 'border-bottom', '1px solid rgba(246, 164, 0, .28)')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('range'),
								$elm$html$Html$Attributes$min('0'),
								$elm$html$Html$Attributes$max(
								$elm$core$String$fromInt(maxId)),
								$elm$html$Html$Attributes$value(
								$elm$core$String$fromInt(selected)),
								$elm$html$Html$Events$onInput($author$project$ElmDev$Debugger$Scrub),
								A2($elm$html$Html$Attributes$style, 'width', '100%'),
								A2($elm$html$Html$Attributes$style, 'accent-color', '#f6a400')
							]),
						_List_Nil)
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'overflow', 'auto'),
						A2($elm$html$Html$Attributes$style, 'padding', '6px 0'),
						A2($elm$html$Html$Attributes$style, 'min-height', '0')
					]),
				A2(
					$elm$core$List$map,
					$author$project$ElmDev$Debugger$viewEventSummary(selected),
					$elm$core$List$reverse(model.events)))
			]));
};
var $author$project$ElmDev$Debugger$view = function (model) {
	return model.open ? A2(
		$elm$html$Html$div,
		$author$project$ElmDev$Debugger$shellStyles,
		_List_fromArray(
			[
				$author$project$ElmDev$Debugger$viewDebuggerHeader(model),
				A2($author$project$ElmDev$Debugger$viewModeTabs, model.viewMode, model.lazyStats),
				function () {
				var _v0 = model.viewMode;
				switch (_v0.$) {
					case 'TimelineView':
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'display', 'flex'),
									A2($elm$html$Html$Attributes$style, 'flex', '1'),
									A2($elm$html$Html$Attributes$style, 'min-height', '0')
								]),
							_List_fromArray(
								[
									$author$project$ElmDev$Debugger$viewTimelineSidebar(model),
									$author$project$ElmDev$Debugger$viewSelected(model)
								]));
					case 'LazyView':
						return $author$project$ElmDev$Debugger$viewLazyStats(model.lazyStats);
					default:
						return $author$project$ElmDev$Debugger$viewLogs(model);
				}
			}()
			])) : A2(
		$elm$html$Html$button,
		$author$project$ElmDev$Debugger$minimizedStyles,
		_List_fromArray(
			[
				$elm$html$Html$text(
				model.inspectMode ? 'Inspecting' : ('Elm Dev (' + ($elm$core$String$fromInt(
					$elm$core$List$length(
						$author$project$ElmDev$Debugger$selectableEvents(model.events))) + ')')))
			]));
};
var $author$project$ElmDev$Debugger$main = $elm$browser$Browser$element(
	{
		init: function (_v0) {
			return _Utils_Tuple2(
				{collapsed: $elm$core$Set$empty, debugLogs: _List_Nil, events: _List_Nil, expanded: $elm$core$Set$empty, inspectMode: false, lazyStats: _List_Nil, messageDrawers: $elm$core$Set$empty, modelView: $author$project$ElmDev$Debugger$ModelDelta, open: false, selected: $elm$core$Maybe$Nothing, selectedLogSource: $elm$core$Maybe$Nothing, viewMode: $author$project$ElmDev$Debugger$TimelineView},
				$author$project$ElmDev$Debugger$setOpen(false));
		},
		subscriptions: function (_v1) {
			return $author$project$ElmDev$Debugger$fromRuntime($author$project$ElmDev$Debugger$RuntimeEvent);
		},
		update: $author$project$ElmDev$Debugger$update,
		view: $author$project$ElmDev$Debugger$view
	});
_Platform_export({'ElmDev':{'Debugger':{'init':$author$project$ElmDev$Debugger$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}}});}(this));