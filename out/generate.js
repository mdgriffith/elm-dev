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

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
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

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
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


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
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
	if (region.t.an === region.aV.an)
	{
		return 'on line ' + region.t.an;
	}
	return 'on lines ' + region.t.an + ' through ' + region.aV.an;
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

	/**_UNUSED/
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

	/**/
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

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
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

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


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



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


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



/**_UNUSED/
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

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

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




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});



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


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.C,
		impl.k_,
		impl.kR,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
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
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

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
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
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
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
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
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
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
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
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


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
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
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

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
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
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
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
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
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
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

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
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


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$project_metadata_utils$Elm$Docs$Module = F6(
	function (name, comment, unions, aliases, values, binops) {
		return {iD: aliases, bR: binops, cd: comment, ao: name, aA: unions, k$: values};
	});
var $elm$project_metadata_utils$Elm$Docs$Alias = F4(
	function (name, comment, args, tipe) {
		return {B: args, cd: comment, ao: name, ay: tipe};
	});
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
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
			if (t.$ === -2) {
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
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
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
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
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
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
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
		return {$: 0, a: a, b: b, c: c, d: d};
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
	return {$: 1, a: a};
};
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
	return {$: 0, a: a};
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
		if (!builder.i) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.k),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.k);
		} else {
			var treeLen = builder.i * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.l) : builder.l;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.i);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.k) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.k);
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
					{l: nodeList, i: (len / $elm$core$Array$branchFactor) | 0, k: tail});
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
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$fail = _Json_fail;
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
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {b8: col, Q: problem, gL: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.gL, p.b8, p.Q);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 0:
					return list;
				case 1:
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0;
		var _v1 = parse(
			{b8: 1, jb: _List_Nil, d: 1, fC: 0, gL: 1, kK: src});
		if (!_v1.$) {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (!_v0.$) {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $elm$parser$Parser$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Forbidden = 0;
var $elm$project_metadata_utils$Elm$Type$Lambda = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Loop = function (a) {
	return {$: 0, a: a};
};
var $elm$project_metadata_utils$Elm$Type$Record = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$project_metadata_utils$Elm$Type$Type = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$project_metadata_utils$Elm$Type$Var = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$parser$Parser$Advanced$Parser = $elm$core$Basics$identity;
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0;
		return function (s0) {
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
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 8, a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {b8: col, jc: contextStack, Q: problem, gL: row};
	});
var $elm$parser$Parser$Advanced$Empty = {$: 0};
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.gL, s.b8, x, s.jb));
	});
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$not = _Basics_not;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.fC, s.gL, s.b8, s.kK);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			$elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{b8: newCol, jb: s.jb, d: s.d, fC: newOffset, gL: newRow, kK: s.kK});
	};
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $elm$project_metadata_utils$Elm$Type$arrow = $elm$parser$Parser$symbol('->');
var $elm$parser$Parser$Advanced$backtrackable = function (_v0) {
	var parse = _v0;
	return function (s0) {
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
var $elm$project_metadata_utils$Elm$Type$comma = $elm$parser$Parser$symbol(',');
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0;
		var parseB = _v1;
		return function (s0) {
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
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p1 || p2,
						A2(func, a, b),
						s2);
				}
			}
		};
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $elm$project_metadata_utils$Elm$Type$isInnerVarChar = function (_char) {
	return $elm$core$Char$isAlphaNum(_char) || (_char === '_');
};
var $elm$parser$Parser$ExpectingVariable = {$: 7};
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
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
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
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
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm$core$String$slice = _String_slice;
var $elm$parser$Parser$Advanced$varHelp = F7(
	function (isGood, offset, row, col, src, indent, context) {
		varHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, src);
			if (_Utils_eq(newOffset, -1)) {
				return {b8: col, jb: context, d: indent, fC: offset, gL: row, kK: src};
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					context = $temp$context;
					continue varHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
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
var $elm$parser$Parser$Advanced$variable = function (i) {
	return function (s) {
		var firstOffset = A3($elm$parser$Parser$Advanced$isSubChar, i.t, s.fC, s.kK);
		if (_Utils_eq(firstOffset, -1)) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, i.c3));
		} else {
			var s1 = _Utils_eq(firstOffset, -2) ? A7($elm$parser$Parser$Advanced$varHelp, i.jM, s.fC + 1, s.gL + 1, 1, s.kK, s.d, s.jb) : A7($elm$parser$Parser$Advanced$varHelp, i.jM, firstOffset, s.gL, s.b8 + 1, s.kK, s.d, s.jb);
			var name = A3($elm$core$String$slice, s.fC, s1.fC, s.kK);
			return A2($elm$core$Set$member, name, i.kr) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, i.c3)) : A3($elm$parser$Parser$Advanced$Good, true, name, s1);
		}
	};
};
var $elm$parser$Parser$variable = function (i) {
	return $elm$parser$Parser$Advanced$variable(
		{c3: $elm$parser$Parser$ExpectingVariable, jM: i.jM, kr: i.kr, t: i.t});
};
var $elm$project_metadata_utils$Elm$Type$var = function (isFirst) {
	return $elm$parser$Parser$variable(
		{jM: $elm$project_metadata_utils$Elm$Type$isInnerVarChar, kr: $elm$core$Set$empty, t: isFirst});
};
var $elm$project_metadata_utils$Elm$Type$lowVar = $elm$project_metadata_utils$Elm$Type$var($elm$core$Char$isLower);
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
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
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
	};
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.kK);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.fC, offset) < 0,
					0,
					{b8: col, jb: s0.jb, d: s0.d, fC: offset, gL: row, kK: s0.kK});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
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
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return function (s) {
		return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.fC, s.gL, s.b8, s);
	};
};
var $elm$parser$Parser$chompWhile = $elm$parser$Parser$Advanced$chompWhile;
var $elm$project_metadata_utils$Elm$Type$spaces = $elm$parser$Parser$chompWhile(
	function (_char) {
		return _char === ' ';
	});
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, a, s);
	};
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $elm$project_metadata_utils$Elm$Type$extension = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($elm$core$Maybe$Just),
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$backtrackable($elm$project_metadata_utils$Elm$Type$lowVar),
						$elm$parser$Parser$backtrackable($elm$project_metadata_utils$Elm$Type$spaces)),
					$elm$parser$Parser$symbol('|')),
				$elm$project_metadata_utils$Elm$Type$spaces)),
			$elm$parser$Parser$succeed($elm$core$Maybe$Nothing)
		]));
var $elm$parser$Parser$Advanced$lazy = function (thunk) {
	return function (s) {
		var _v0 = thunk(0);
		var parse = _v0;
		return parse(s);
	};
};
var $elm$parser$Parser$lazy = $elm$parser$Parser$Advanced$lazy;
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
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
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
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
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return function (s) {
			return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
		};
	});
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					func(a),
					s1);
			} else {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			}
		};
	});
var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$toAdvancedStep = function (step) {
	if (!step.$) {
		var s = step.a;
		return $elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return $elm$parser$Parser$Advanced$Done(a);
	}
};
var $elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			$elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$project_metadata_utils$Elm$Type$capVar = $elm$project_metadata_utils$Elm$Type$var($elm$core$Char$isUpper);
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					A2(
						func,
						A3($elm$core$String$slice, s0.fC, s1.fC, s0.kK),
						a),
					s1);
			}
		};
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$getChompedString = $elm$parser$Parser$Advanced$getChompedString;
var $elm$project_metadata_utils$Elm$Type$qualifiedCapVarHelp = function (_v0) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(
						$elm$parser$Parser$Loop(0)),
					$elm$parser$Parser$symbol('.')),
				$elm$project_metadata_utils$Elm$Type$capVar),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(0))
			]));
};
var $elm$project_metadata_utils$Elm$Type$qualifiedCapVar = $elm$parser$Parser$getChompedString(
	A2(
		$elm$parser$Parser$ignorer,
		$elm$project_metadata_utils$Elm$Type$capVar,
		A2($elm$parser$Parser$loop, 0, $elm$project_metadata_utils$Elm$Type$qualifiedCapVarHelp)));
var $elm$parser$Parser$Advanced$revAlways = F2(
	function (_v0, b) {
		return b;
	});
var $elm$parser$Parser$Advanced$skip = F2(
	function (iParser, kParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$parser$Parser$Advanced$revAlways, iParser, kParser);
	});
var $elm$parser$Parser$Advanced$sequenceEndForbidden = F5(
	function (ender, ws, parseItem, sep, revItems) {
		var chompRest = function (item) {
			return A5(
				$elm$parser$Parser$Advanced$sequenceEndForbidden,
				ender,
				ws,
				parseItem,
				sep,
				A2($elm$core$List$cons, item, revItems));
		};
		return A2(
			$elm$parser$Parser$Advanced$skip,
			ws,
			$elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$Advanced$skip,
						sep,
						A2(
							$elm$parser$Parser$Advanced$skip,
							ws,
							A2(
								$elm$parser$Parser$Advanced$map,
								function (item) {
									return $elm$parser$Parser$Advanced$Loop(
										A2($elm$core$List$cons, item, revItems));
								},
								parseItem))),
						A2(
						$elm$parser$Parser$Advanced$map,
						function (_v0) {
							return $elm$parser$Parser$Advanced$Done(
								$elm$core$List$reverse(revItems));
						},
						ender)
					])));
	});
var $elm$parser$Parser$Advanced$sequenceEndMandatory = F4(
	function (ws, parseItem, sep, revItems) {
		return $elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$Advanced$map,
					function (item) {
						return $elm$parser$Parser$Advanced$Loop(
							A2($elm$core$List$cons, item, revItems));
					},
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						parseItem,
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							ws,
							A2($elm$parser$Parser$Advanced$ignorer, sep, ws)))),
					A2(
					$elm$parser$Parser$Advanced$map,
					function (_v0) {
						return $elm$parser$Parser$Advanced$Done(
							$elm$core$List$reverse(revItems));
					},
					$elm$parser$Parser$Advanced$succeed(0))
				]));
	});
var $elm$parser$Parser$Advanced$sequenceEndOptional = F5(
	function (ender, ws, parseItem, sep, revItems) {
		var parseEnd = A2(
			$elm$parser$Parser$Advanced$map,
			function (_v0) {
				return $elm$parser$Parser$Advanced$Done(
					$elm$core$List$reverse(revItems));
			},
			ender);
		return A2(
			$elm$parser$Parser$Advanced$skip,
			ws,
			$elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$Advanced$skip,
						sep,
						A2(
							$elm$parser$Parser$Advanced$skip,
							ws,
							$elm$parser$Parser$Advanced$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$parser$Parser$Advanced$map,
										function (item) {
											return $elm$parser$Parser$Advanced$Loop(
												A2($elm$core$List$cons, item, revItems));
										},
										parseItem),
										parseEnd
									])))),
						parseEnd
					])));
	});
var $elm$parser$Parser$Advanced$sequenceEnd = F5(
	function (ender, ws, parseItem, sep, trailing) {
		var chompRest = function (item) {
			switch (trailing) {
				case 0:
					return A2(
						$elm$parser$Parser$Advanced$loop,
						_List_fromArray(
							[item]),
						A4($elm$parser$Parser$Advanced$sequenceEndForbidden, ender, ws, parseItem, sep));
				case 1:
					return A2(
						$elm$parser$Parser$Advanced$loop,
						_List_fromArray(
							[item]),
						A4($elm$parser$Parser$Advanced$sequenceEndOptional, ender, ws, parseItem, sep));
				default:
					return A2(
						$elm$parser$Parser$Advanced$ignorer,
						A2(
							$elm$parser$Parser$Advanced$skip,
							ws,
							A2(
								$elm$parser$Parser$Advanced$skip,
								sep,
								A2(
									$elm$parser$Parser$Advanced$skip,
									ws,
									A2(
										$elm$parser$Parser$Advanced$loop,
										_List_fromArray(
											[item]),
										A3($elm$parser$Parser$Advanced$sequenceEndMandatory, ws, parseItem, sep))))),
						ender);
			}
		};
		return $elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2($elm$parser$Parser$Advanced$andThen, chompRest, parseItem),
					A2(
					$elm$parser$Parser$Advanced$map,
					function (_v0) {
						return _List_Nil;
					},
					ender)
				]));
	});
var $elm$parser$Parser$Advanced$sequence = function (i) {
	return A2(
		$elm$parser$Parser$Advanced$skip,
		$elm$parser$Parser$Advanced$token(i.t),
		A2(
			$elm$parser$Parser$Advanced$skip,
			i.g7,
			A5(
				$elm$parser$Parser$Advanced$sequenceEnd,
				$elm$parser$Parser$Advanced$token(i.aV),
				i.g7,
				i.jR,
				$elm$parser$Parser$Advanced$token(i.kA),
				i.bd)));
};
var $elm$parser$Parser$Advanced$Forbidden = 0;
var $elm$parser$Parser$Advanced$Mandatory = 2;
var $elm$parser$Parser$Advanced$Optional = 1;
var $elm$parser$Parser$toAdvancedTrailing = function (trailing) {
	switch (trailing) {
		case 0:
			return 0;
		case 1:
			return 1;
		default:
			return 2;
	}
};
var $elm$parser$Parser$Expecting = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$toToken = function (str) {
	return A2(
		$elm$parser$Parser$Advanced$Token,
		str,
		$elm$parser$Parser$Expecting(str));
};
var $elm$parser$Parser$sequence = function (i) {
	return $elm$parser$Parser$Advanced$sequence(
		{
			aV: $elm$parser$Parser$toToken(i.aV),
			jR: i.jR,
			kA: $elm$parser$Parser$toToken(i.kA),
			g7: i.g7,
			t: $elm$parser$Parser$toToken(i.t),
			bd: $elm$parser$Parser$toAdvancedTrailing(i.bd)
		});
};
var $elm$project_metadata_utils$Elm$Type$Tuple = function (a) {
	return {$: 2, a: a};
};
var $elm$project_metadata_utils$Elm$Type$tuplize = function (args) {
	if (args.b && (!args.b.b)) {
		var arg = args.a;
		return arg;
	} else {
		return $elm$project_metadata_utils$Elm$Type$Tuple(args);
	}
};
var $elm$project_metadata_utils$Elm$Type$chompArgs = function (revArgs) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (arg) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, arg, revArgs));
				},
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed($elm$core$Basics$identity),
						$elm$parser$Parser$backtrackable($elm$project_metadata_utils$Elm$Type$spaces)),
					$elm$project_metadata_utils$Elm$Type$cyclic$term())),
				A2(
				$elm$parser$Parser$map,
				function (_v2) {
					return $elm$parser$Parser$Done(
						$elm$core$List$reverse(revArgs));
				},
				$elm$parser$Parser$succeed(0))
			]));
};
var $elm$project_metadata_utils$Elm$Type$recordEndHelp = function (revFields) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed(
							function (f) {
								return $elm$parser$Parser$Loop(
									A2($elm$core$List$cons, f, revFields));
							}),
						$elm$project_metadata_utils$Elm$Type$comma),
					$elm$project_metadata_utils$Elm$Type$spaces),
				A2(
					$elm$parser$Parser$ignorer,
					$elm$project_metadata_utils$Elm$Type$cyclic$field(),
					$elm$project_metadata_utils$Elm$Type$spaces)),
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					function (_v1) {
						return $elm$parser$Parser$Done(
							$elm$core$List$reverse(revFields));
					}),
				$elm$parser$Parser$symbol('}'))
			]));
};
var $elm$project_metadata_utils$Elm$Type$tipeHelp = function (t) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				$elm$project_metadata_utils$Elm$Type$Lambda(t),
				$elm$project_metadata_utils$Elm$Type$cyclic$arrowAndType()),
				$elm$parser$Parser$succeed(t)
			]));
};
function $elm$project_metadata_utils$Elm$Type$cyclic$arrowAndType() {
	return A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed($elm$core$Basics$identity),
					$elm$parser$Parser$backtrackable($elm$project_metadata_utils$Elm$Type$spaces)),
				$elm$project_metadata_utils$Elm$Type$arrow),
			$elm$project_metadata_utils$Elm$Type$spaces),
		$elm$project_metadata_utils$Elm$Type$cyclic$tipe());
}
function $elm$project_metadata_utils$Elm$Type$cyclic$tipeTerm() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2($elm$parser$Parser$map, $elm$project_metadata_utils$Elm$Type$Var, $elm$project_metadata_utils$Elm$Type$lowVar),
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$keeper,
					$elm$parser$Parser$succeed($elm$project_metadata_utils$Elm$Type$Type),
					$elm$project_metadata_utils$Elm$Type$qualifiedCapVar),
				A2($elm$parser$Parser$loop, _List_Nil, $elm$project_metadata_utils$Elm$Type$chompArgs)),
				$elm$project_metadata_utils$Elm$Type$cyclic$record(),
				$elm$project_metadata_utils$Elm$Type$cyclic$tuple()
			]));
}
function $elm$project_metadata_utils$Elm$Type$cyclic$term() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2($elm$parser$Parser$map, $elm$project_metadata_utils$Elm$Type$Var, $elm$project_metadata_utils$Elm$Type$lowVar),
				A2(
				$elm$parser$Parser$map,
				function (name) {
					return A2($elm$project_metadata_utils$Elm$Type$Type, name, _List_Nil);
				},
				$elm$project_metadata_utils$Elm$Type$qualifiedCapVar),
				$elm$project_metadata_utils$Elm$Type$cyclic$record(),
				$elm$project_metadata_utils$Elm$Type$cyclic$tuple()
			]));
}
function $elm$project_metadata_utils$Elm$Type$cyclic$record() {
	return A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(
						F2(
							function (ext, fs) {
								return A2($elm$project_metadata_utils$Elm$Type$Record, fs, ext);
							})),
					$elm$parser$Parser$symbol('{')),
				$elm$project_metadata_utils$Elm$Type$spaces),
			$elm$project_metadata_utils$Elm$Type$extension),
		$elm$project_metadata_utils$Elm$Type$cyclic$recordEnd());
}
function $elm$project_metadata_utils$Elm$Type$cyclic$recordEnd() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$andThen,
				function (f) {
					return A2(
						$elm$parser$Parser$loop,
						_List_fromArray(
							[f]),
						$elm$project_metadata_utils$Elm$Type$recordEndHelp);
				},
				A2(
					$elm$parser$Parser$ignorer,
					$elm$project_metadata_utils$Elm$Type$cyclic$field(),
					$elm$project_metadata_utils$Elm$Type$spaces)),
				A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed(_List_Nil),
				$elm$parser$Parser$symbol('}'))
			]));
}
function $elm$project_metadata_utils$Elm$Type$cyclic$field() {
	return A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($elm$core$Tuple$pair),
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					A2($elm$parser$Parser$ignorer, $elm$project_metadata_utils$Elm$Type$lowVar, $elm$project_metadata_utils$Elm$Type$spaces),
					$elm$parser$Parser$symbol(':')),
				$elm$project_metadata_utils$Elm$Type$spaces)),
		$elm$project_metadata_utils$Elm$Type$cyclic$tipe());
}
function $elm$project_metadata_utils$Elm$Type$cyclic$tuple() {
	return A2(
		$elm$parser$Parser$map,
		$elm$project_metadata_utils$Elm$Type$tuplize,
		$elm$parser$Parser$sequence(
			{
				aV: ')',
				jR: $elm$project_metadata_utils$Elm$Type$cyclic$tipe(),
				kA: ',',
				g7: $elm$project_metadata_utils$Elm$Type$spaces,
				t: '(',
				bd: 0
			}));
}
function $elm$project_metadata_utils$Elm$Type$cyclic$tipe() {
	return $elm$parser$Parser$lazy(
		function (_v0) {
			return A2(
				$elm$parser$Parser$andThen,
				$elm$project_metadata_utils$Elm$Type$tipeHelp,
				$elm$project_metadata_utils$Elm$Type$cyclic$tipeTerm());
		});
}
var $elm$project_metadata_utils$Elm$Type$arrowAndType = $elm$project_metadata_utils$Elm$Type$cyclic$arrowAndType();
$elm$project_metadata_utils$Elm$Type$cyclic$arrowAndType = function () {
	return $elm$project_metadata_utils$Elm$Type$arrowAndType;
};
var $elm$project_metadata_utils$Elm$Type$tipeTerm = $elm$project_metadata_utils$Elm$Type$cyclic$tipeTerm();
$elm$project_metadata_utils$Elm$Type$cyclic$tipeTerm = function () {
	return $elm$project_metadata_utils$Elm$Type$tipeTerm;
};
var $elm$project_metadata_utils$Elm$Type$term = $elm$project_metadata_utils$Elm$Type$cyclic$term();
$elm$project_metadata_utils$Elm$Type$cyclic$term = function () {
	return $elm$project_metadata_utils$Elm$Type$term;
};
var $elm$project_metadata_utils$Elm$Type$record = $elm$project_metadata_utils$Elm$Type$cyclic$record();
$elm$project_metadata_utils$Elm$Type$cyclic$record = function () {
	return $elm$project_metadata_utils$Elm$Type$record;
};
var $elm$project_metadata_utils$Elm$Type$recordEnd = $elm$project_metadata_utils$Elm$Type$cyclic$recordEnd();
$elm$project_metadata_utils$Elm$Type$cyclic$recordEnd = function () {
	return $elm$project_metadata_utils$Elm$Type$recordEnd;
};
var $elm$project_metadata_utils$Elm$Type$field = $elm$project_metadata_utils$Elm$Type$cyclic$field();
$elm$project_metadata_utils$Elm$Type$cyclic$field = function () {
	return $elm$project_metadata_utils$Elm$Type$field;
};
var $elm$project_metadata_utils$Elm$Type$tuple = $elm$project_metadata_utils$Elm$Type$cyclic$tuple();
$elm$project_metadata_utils$Elm$Type$cyclic$tuple = function () {
	return $elm$project_metadata_utils$Elm$Type$tuple;
};
var $elm$project_metadata_utils$Elm$Type$tipe = $elm$project_metadata_utils$Elm$Type$cyclic$tipe();
$elm$project_metadata_utils$Elm$Type$cyclic$tipe = function () {
	return $elm$project_metadata_utils$Elm$Type$tipe;
};
var $elm$project_metadata_utils$Elm$Type$parse = function (source) {
	return A2($elm$parser$Parser$run, $elm$project_metadata_utils$Elm$Type$tipe, source);
};
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$project_metadata_utils$Elm$Type$decoderHelp = function (string) {
	var _v0 = $elm$project_metadata_utils$Elm$Type$parse(string);
	if (_v0.$ === 1) {
		var error = _v0.a;
		return $elm$json$Json$Decode$fail('TODO');
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
var $elm$project_metadata_utils$Elm$Docs$aliasDecoder = A5(
	$elm$json$Json$Decode$map4,
	$elm$project_metadata_utils$Elm$Docs$Alias,
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'comment', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'args',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'type', $elm$project_metadata_utils$Elm$Type$decoder));
var $elm$project_metadata_utils$Elm$Docs$Binop = F5(
	function (name, comment, tipe, associativity, precedence) {
		return {iK: associativity, cd: comment, ao: name, kl: precedence, ay: tipe};
	});
var $elm$project_metadata_utils$Elm$Docs$Left = 0;
var $elm$project_metadata_utils$Elm$Docs$None = 1;
var $elm$project_metadata_utils$Elm$Docs$Right = 2;
var $elm$project_metadata_utils$Elm$Docs$toAssoc = function (str) {
	switch (str) {
		case 'left':
			return $elm$json$Json$Decode$succeed(0);
		case 'non':
			return $elm$json$Json$Decode$succeed(1);
		case 'right':
			return $elm$json$Json$Decode$succeed(2);
		default:
			return $elm$json$Json$Decode$fail('expecting one of the following values: left, non, right');
	}
};
var $elm$project_metadata_utils$Elm$Docs$assocDecoder = A2($elm$json$Json$Decode$andThen, $elm$project_metadata_utils$Elm$Docs$toAssoc, $elm$json$Json$Decode$string);
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map5 = _Json_map5;
var $elm$project_metadata_utils$Elm$Docs$binopDecoder = A6(
	$elm$json$Json$Decode$map5,
	$elm$project_metadata_utils$Elm$Docs$Binop,
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'comment', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'type', $elm$project_metadata_utils$Elm$Type$decoder),
	A2($elm$json$Json$Decode$field, 'associativity', $elm$project_metadata_utils$Elm$Docs$assocDecoder),
	A2($elm$json$Json$Decode$field, 'precedence', $elm$json$Json$Decode$int));
var $elm$json$Json$Decode$map6 = _Json_map6;
var $elm$project_metadata_utils$Elm$Docs$Union = F4(
	function (name, comment, args, tags) {
		return {B: args, cd: comment, ao: name, ax: tags};
	});
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$project_metadata_utils$Elm$Docs$tagDecoder = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$index,
		1,
		$elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Type$decoder)));
var $elm$project_metadata_utils$Elm$Docs$unionDecoder = A5(
	$elm$json$Json$Decode$map4,
	$elm$project_metadata_utils$Elm$Docs$Union,
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'comment', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'args',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$field,
		'cases',
		$elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$tagDecoder)));
var $elm$project_metadata_utils$Elm$Docs$Value = F3(
	function (name, comment, tipe) {
		return {cd: comment, ao: name, ay: tipe};
	});
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$project_metadata_utils$Elm$Docs$valueDecoder = A4(
	$elm$json$Json$Decode$map3,
	$elm$project_metadata_utils$Elm$Docs$Value,
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'comment', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'type', $elm$project_metadata_utils$Elm$Type$decoder));
var $elm$project_metadata_utils$Elm$Docs$decoder = A7(
	$elm$json$Json$Decode$map6,
	$elm$project_metadata_utils$Elm$Docs$Module,
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'comment', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'unions',
		$elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$unionDecoder)),
	A2(
		$elm$json$Json$Decode$field,
		'aliases',
		$elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$aliasDecoder)),
	A2(
		$elm$json$Json$Decode$field,
		'values',
		$elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$valueDecoder)),
	A2(
		$elm$json$Json$Decode$field,
		'binops',
		$elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$binopDecoder)));
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
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
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
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
			_Json_emptyObject(0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Gen$CodeGen$Generate$onFailureSend = _Platform_outgoingPort(
	'onFailureSend',
	$elm$json$Json$Encode$list(
		function ($) {
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'description',
						$elm$json$Json$Encode$string($.aS)),
						_Utils_Tuple2(
						'title',
						$elm$json$Json$Encode$string($.kT))
					]));
		}));
var $author$project$Gen$CodeGen$Generate$error = function (errs) {
	return $author$project$Gen$CodeGen$Generate$onFailureSend(errs);
};
var $author$project$Gen$CodeGen$Generate$onSuccessSend = _Platform_outgoingPort(
	'onSuccessSend',
	$elm$json$Json$Encode$list(
		function ($) {
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'contents',
						$elm$json$Json$Encode$string($.cj)),
						_Utils_Tuple2(
						'path',
						$elm$json$Json$Encode$string($.f4)),
						_Utils_Tuple2(
						'warnings',
						$elm$json$Json$Encode$list(
							function ($) {
								return $elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'declaration',
											$elm$json$Json$Encode$string($.jf)),
											_Utils_Tuple2(
											'warning',
											$elm$json$Json$Encode$string($.k0))
										]));
							})($.io))
					]));
		}));
var $author$project$Gen$CodeGen$Generate$files = function (list) {
	return $author$project$Gen$CodeGen$Generate$onSuccessSend(list);
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$core$Platform$worker = _Platform_worker;
var $author$project$Gen$CodeGen$Generate$fromJson = F2(
	function (decoder, f) {
		return $elm$core$Platform$worker(
			{
				C: function (flags) {
					var _v0 = A2($elm$json$Json$Decode$decodeValue, decoder, flags);
					if (!_v0.$) {
						var input = _v0.a;
						return _Utils_Tuple2(
							0,
							$author$project$Gen$CodeGen$Generate$files(
								f(input)));
					} else {
						var e = _v0.a;
						return _Utils_Tuple2(
							0,
							$author$project$Gen$CodeGen$Generate$error(
								_List_fromArray(
									[
										{
										aS: $elm$json$Json$Decode$errorToString(e),
										kT: 'Error decoding flags'
									}
									])));
					}
				},
				kR: function (_v1) {
					return $elm$core$Platform$Sub$none;
				},
				k_: F2(
					function (_v2, model) {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					})
			});
	});
var $mdgriffith$elm_codegen$Internal$Compiler$Declaration = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$NotExposed = {$: 0};
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (!result.$) {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Node$Node = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record = function (a) {
	return {$: 4, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled = function (a) {
	return {$: 3, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Node$map = F2(
	function (f, _v0) {
		var r = _v0.a;
		var a = _v0.b;
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			r,
			f(a));
	});
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $mdgriffith$elm_codegen$Internal$Clean$doRename = F2(
	function (dict, ann) {
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
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
					name,
					A2(
						$elm$core$List$map,
						$stil4m$elm_syntax$Elm$Syntax$Node$map(
							$mdgriffith$elm_codegen$Internal$Clean$doRename(dict)),
						nodedVars));
			case 2:
				return ann;
			case 3:
				var nodedVars = ann.a;
				return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(
					A2(
						$elm$core$List$map,
						$stil4m$elm_syntax$Elm$Syntax$Node$map(
							$mdgriffith$elm_codegen$Internal$Clean$doRename(dict)),
						nodedVars));
			case 4:
				var record = ann.a;
				return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(
					A2(
						$elm$core$List$map,
						$stil4m$elm_syntax$Elm$Syntax$Node$map(
							$elm$core$Tuple$mapSecond(
								$stil4m$elm_syntax$Elm$Syntax$Node$map(
									$mdgriffith$elm_codegen$Internal$Clean$doRename(dict)))),
						record));
			case 5:
				var name = ann.a;
				var _v2 = ann.b;
				var range = _v2.a;
				var record = _v2.b;
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord,
					name,
					A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						range,
						A2(
							$elm$core$List$map,
							$stil4m$elm_syntax$Elm$Syntax$Node$map(
								$elm$core$Tuple$mapSecond(
									$stil4m$elm_syntax$Elm$Syntax$Node$map(
										$mdgriffith$elm_codegen$Internal$Clean$doRename(dict)))),
							record)));
			default:
				var nodeOne = ann.a;
				var nodeTwo = ann.b;
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
					A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$map,
						$mdgriffith$elm_codegen$Internal$Clean$doRename(dict),
						nodeOne),
					A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$map,
						$mdgriffith$elm_codegen$Internal$Clean$doRename(dict),
						nodeTwo));
		}
	});
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
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
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
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
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
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
		if ((_v0.$ === -1) && (!_v0.a)) {
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
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $mdgriffith$elm_codegen$Internal$Clean$prepareRename = F2(
	function (ann, dict) {
		switch (ann.$) {
			case 0:
				var generic = ann.a;
				return A2($elm$core$Set$insert, generic, dict);
			case 1:
				var name = ann.a;
				var nodedVars = ann.b;
				return A3(
					$elm$core$List$foldl,
					F2(
						function (_v1, d) {
							var tipe = _v1.b;
							return A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, tipe, d);
						}),
					dict,
					nodedVars);
			case 2:
				return dict;
			case 3:
				var nodedVars = ann.a;
				return A3(
					$elm$core$List$foldl,
					F2(
						function (_v2, d) {
							var tipe = _v2.b;
							return A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, tipe, d);
						}),
					dict,
					nodedVars);
			case 4:
				var record = ann.a;
				return A3(
					$elm$core$List$foldl,
					F2(
						function (_v3, d) {
							var _v4 = _v3.b;
							var _v5 = _v4.b;
							var field = _v5.b;
							return A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, field, d);
						}),
					dict,
					record);
			case 5:
				var name = ann.a;
				var _v6 = ann.b;
				var range = _v6.a;
				var record = _v6.b;
				return A3(
					$elm$core$List$foldl,
					F2(
						function (_v7, d) {
							var _v8 = _v7.b;
							var _v9 = _v8.b;
							var field = _v9.b;
							return A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, field, d);
						}),
					dict,
					record);
			default:
				var _v10 = ann.a;
				var one = _v10.b;
				var _v11 = ann.b;
				var two = _v11.b;
				return A2(
					$mdgriffith$elm_codegen$Internal$Clean$prepareRename,
					two,
					A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, one, dict));
		}
	});
var $mdgriffith$elm_codegen$Internal$Clean$findClean = F3(
	function (i, name, set) {
		findClean:
		while (true) {
			var newName = (!i) ? name : _Utils_ap(
				name,
				$elm$core$String$fromInt(i));
			if (A2($elm$core$Set$member, newName, set)) {
				var $temp$i = i + 1,
					$temp$name = name,
					$temp$set = set;
				i = $temp$i;
				name = $temp$name;
				set = $temp$set;
				continue findClean;
			} else {
				return name;
			}
		}
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Set$foldl = F3(
	function (func, initialState, _v0) {
		var dict = _v0;
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (key, _v1, state) {
					return A2(func, key, state);
				}),
			initialState,
			dict);
	});
var $mdgriffith$elm_codegen$Internal$Clean$sanitized = function (str) {
	var _v0 = A2($elm$core$String$split, '_', str);
	if (!_v0.b) {
		return str;
	} else {
		var top = _v0.a;
		var remain = _v0.b;
		return top;
	}
};
var $mdgriffith$elm_codegen$Internal$Clean$verify = function (set) {
	return A3(
		$elm$core$Set$foldl,
		F2(
			function (name, gathered) {
				var newName = A3(
					$mdgriffith$elm_codegen$Internal$Clean$findClean,
					0,
					$mdgriffith$elm_codegen$Internal$Clean$sanitized(name),
					set);
				return A3($elm$core$Dict$insert, name, newName, gathered);
			}),
		$elm$core$Dict$empty,
		set);
};
var $mdgriffith$elm_codegen$Internal$Clean$clean = function (ann) {
	var renames = $mdgriffith$elm_codegen$Internal$Clean$verify(
		A2($mdgriffith$elm_codegen$Internal$Clean$prepareRename, ann, $elm$core$Set$empty));
	return A2($mdgriffith$elm_codegen$Internal$Clean$doRename, renames, ann);
};
var $elm$core$String$length = _String_length;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $mdgriffith$elm_codegen$Internal$Format$sanitize = function (str) {
	switch (str) {
		case 'in':
			return 'in_';
		case 'type':
			return 'type_';
		case 'case':
			return 'case_';
		case 'let':
			return 'let_';
		case 'module':
			return 'module_';
		case 'exposing':
			return 'exposing_';
		case 'where':
			return 'where_';
		case 'main':
			return 'main_';
		case 'port':
			return 'port_';
		case 'as':
			return 'as_';
		case 'if':
			return 'if_';
		case 'import':
			return 'import_';
		default:
			return str;
	}
};
var $elm$core$String$toLower = _String_toLower;
var $mdgriffith$elm_codegen$Internal$Format$formatValue = function (str) {
	var formatted = _Utils_ap(
		$elm$core$String$toLower(
			A2($elm$core$String$left, 1, str)),
		A2($elm$core$String$dropLeft, 1, str));
	return $mdgriffith$elm_codegen$Internal$Format$sanitize(formatted);
};
var $mdgriffith$elm_codegen$Internal$Format$formatDeclarationName = function (str) {
	if (str === 'main') {
		return 'main';
	} else {
		return $mdgriffith$elm_codegen$Internal$Format$formatValue(str);
	}
};
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange = {
	aV: {aD: 0, gL: 0},
	t: {aD: 0, gL: 0}
};
var $mdgriffith$elm_codegen$Internal$Compiler$nodify = function (exp) {
	return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, exp);
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $stil4m$structured_writer$StructuredWriter$asIndent = function (amount) {
	return A2($elm$core$String$repeat, amount, ' ');
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
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
var $stil4m$structured_writer$StructuredWriter$writeIndented = F2(
	function (indent_, w) {
		switch (w.$) {
			case 0:
				var _v1 = w.a;
				var pre = _v1.a;
				var sep = _v1.b;
				var post = _v1.c;
				var differentLines = w.b;
				var items = w.c;
				var seperator = differentLines ? ('\n' + ($stil4m$structured_writer$StructuredWriter$asIndent(indent_) + sep)) : sep;
				return $elm$core$String$concat(
					_List_fromArray(
						[
							pre,
							A2(
							$elm$core$String$join,
							seperator,
							A2(
								$elm$core$List$map,
								A2(
									$elm$core$Basics$composeR,
									$elm$core$Basics$identity,
									$stil4m$structured_writer$StructuredWriter$writeIndented(indent_)),
								items)),
							post
						]));
			case 1:
				var items = w.a;
				return A2(
					$elm$core$String$join,
					'\n' + $stil4m$structured_writer$StructuredWriter$asIndent(indent_),
					A2(
						$elm$core$List$concatMap,
						A2(
							$elm$core$Basics$composeR,
							$stil4m$structured_writer$StructuredWriter$writeIndented(0),
							$elm$core$String$split('\n')),
						items));
			case 2:
				var s = w.a;
				return s;
			case 4:
				var n = w.a;
				var next = w.b;
				return _Utils_ap(
					$stil4m$structured_writer$StructuredWriter$asIndent(n + indent_),
					A2($stil4m$structured_writer$StructuredWriter$writeIndented, n + indent_, next));
			case 5:
				var items = w.a;
				return A2(
					$elm$core$String$join,
					' ',
					A2(
						$elm$core$List$map,
						$stil4m$structured_writer$StructuredWriter$writeIndented(indent_),
						items));
			case 6:
				var items = w.a;
				return $elm$core$String$concat(
					A2(
						$elm$core$List$map,
						$stil4m$structured_writer$StructuredWriter$writeIndented(indent_),
						items));
			default:
				var x = w.a;
				var y = w.b;
				return _Utils_ap(
					A2($stil4m$structured_writer$StructuredWriter$writeIndented, indent_, x),
					A2($stil4m$structured_writer$StructuredWriter$writeIndented, indent_, y));
		}
	});
var $stil4m$structured_writer$StructuredWriter$write = $stil4m$structured_writer$StructuredWriter$writeIndented(0);
var $stil4m$elm_syntax$Elm$Writer$write = $stil4m$structured_writer$StructuredWriter$write;
var $stil4m$structured_writer$StructuredWriter$Sep = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $stil4m$structured_writer$StructuredWriter$bracesComma = $stil4m$structured_writer$StructuredWriter$Sep(
	_Utils_Tuple3('{', ', ', '}'));
var $stil4m$structured_writer$StructuredWriter$Joined = function (a) {
	return {$: 6, a: a};
};
var $stil4m$structured_writer$StructuredWriter$join = $stil4m$structured_writer$StructuredWriter$Joined;
var $stil4m$structured_writer$StructuredWriter$parensComma = $stil4m$structured_writer$StructuredWriter$Sep(
	_Utils_Tuple3('(', ', ', ')'));
var $elm$core$String$contains = _String_contains;
var $stil4m$structured_writer$StructuredWriter$Str = function (a) {
	return {$: 2, a: a};
};
var $stil4m$structured_writer$StructuredWriter$string = $stil4m$structured_writer$StructuredWriter$Str;
var $stil4m$elm_syntax$Elm$Writer$parensIfContainsSpaces = function (w) {
	return A2(
		$elm$core$String$contains,
		' ',
		$stil4m$structured_writer$StructuredWriter$write(w)) ? $stil4m$structured_writer$StructuredWriter$join(
		_List_fromArray(
			[
				$stil4m$structured_writer$StructuredWriter$string('('),
				w,
				$stil4m$structured_writer$StructuredWriter$string(')')
			])) : w;
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $stil4m$structured_writer$StructuredWriter$sepByComma = $stil4m$structured_writer$StructuredWriter$Sep(
	_Utils_Tuple3('', ', ', ''));
var $stil4m$structured_writer$StructuredWriter$Spaced = function (a) {
	return {$: 5, a: a};
};
var $stil4m$structured_writer$StructuredWriter$spaced = $stil4m$structured_writer$StructuredWriter$Spaced;
var $stil4m$elm_syntax$Elm$Syntax$Node$value = function (_v0) {
	var v = _v0.b;
	return v;
};
var $stil4m$elm_syntax$Elm$Writer$writeRecordField = function (_v4) {
	var _v5 = _v4.b;
	var name = _v5.a;
	var ref = _v5.b;
	return $stil4m$structured_writer$StructuredWriter$spaced(
		_List_fromArray(
			[
				$stil4m$structured_writer$StructuredWriter$string(
				$stil4m$elm_syntax$Elm$Syntax$Node$value(name)),
				$stil4m$structured_writer$StructuredWriter$string(':'),
				$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(ref)
			]));
};
var $stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation = function (_v0) {
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
			return $stil4m$structured_writer$StructuredWriter$spaced(
				A2(
					$elm$core$List$cons,
					$stil4m$structured_writer$StructuredWriter$string(
						A2(
							$elm$core$String$join,
							'.',
							_Utils_ap(
								moduleName,
								_List_fromArray(
									[k])))),
					A2(
						$elm$core$List$map,
						A2($elm$core$Basics$composeR, $stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation, $stil4m$elm_syntax$Elm$Writer$parensIfContainsSpaces),
						args)));
		case 2:
			return $stil4m$structured_writer$StructuredWriter$string('()');
		case 3:
			var xs = typeAnnotation.a;
			return A2(
				$stil4m$structured_writer$StructuredWriter$parensComma,
				false,
				A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation, xs));
		case 4:
			var xs = typeAnnotation.a;
			return A2(
				$stil4m$structured_writer$StructuredWriter$bracesComma,
				false,
				A2($elm$core$List$map, $stil4m$elm_syntax$Elm$Writer$writeRecordField, xs));
		case 5:
			var name = typeAnnotation.a;
			var fields = typeAnnotation.b;
			return $stil4m$structured_writer$StructuredWriter$spaced(
				_List_fromArray(
					[
						$stil4m$structured_writer$StructuredWriter$string('{'),
						$stil4m$structured_writer$StructuredWriter$string(
						$stil4m$elm_syntax$Elm$Syntax$Node$value(name)),
						$stil4m$structured_writer$StructuredWriter$string('|'),
						A2(
						$stil4m$structured_writer$StructuredWriter$sepByComma,
						false,
						A2(
							$elm$core$List$map,
							$stil4m$elm_syntax$Elm$Writer$writeRecordField,
							$stil4m$elm_syntax$Elm$Syntax$Node$value(fields))),
						$stil4m$structured_writer$StructuredWriter$string('}')
					]));
		default:
			var left = typeAnnotation.a;
			var right = typeAnnotation.b;
			var addParensForSubTypeAnnotation = function (type_) {
				if (type_.b.$ === 6) {
					var _v3 = type_.b;
					return $stil4m$structured_writer$StructuredWriter$join(
						_List_fromArray(
							[
								$stil4m$structured_writer$StructuredWriter$string('('),
								$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(type_),
								$stil4m$structured_writer$StructuredWriter$string(')')
							]));
				} else {
					return $stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(type_);
				}
			};
			return $stil4m$structured_writer$StructuredWriter$spaced(
				_List_fromArray(
					[
						addParensForSubTypeAnnotation(left),
						$stil4m$structured_writer$StructuredWriter$string('->'),
						addParensForSubTypeAnnotation(right)
					]));
	}
};
var $mdgriffith$elm_codegen$Internal$Compiler$inferenceErrorToString = function (inf) {
	switch (inf.$) {
		case 1:
			var str = inf.a;
			return 'Todo ' + str;
		case 0:
			var one = inf.a;
			var two = inf.b;
			return 'There are multiple different types in a list: \n\n' + ('    ' + ($stil4m$elm_syntax$Elm$Writer$write(
				$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(one))) + ('\n\n    ' + $stil4m$elm_syntax$Elm$Writer$write(
				$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(two))))));
		case 11:
			var details = inf.a;
			return 'Mismatched record update';
		case 2:
			return 'Case statement is empty';
		case 3:
			var fn = inf.a;
			var args = inf.b;
			return 'The following is being called as a function\n\n    ' + ($stil4m$elm_syntax$Elm$Writer$write(
				$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(fn))) + ('\n\nwith these arguments:\n\n    ' + (A2(
				$elm$core$String$join,
				' -> ',
				A2(
					$elm$core$List$map,
					function (arg) {
						return $stil4m$elm_syntax$Elm$Writer$write(
							$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(arg)));
					},
					args)) + '\n\nbut that\'s wrong, right?')));
		case 5:
			var fieldName = inf.a;
			return 'There is a duplicate field in a record: ' + fieldName;
		case 6:
			return 'Case returns different types.';
		case 7:
			var found = inf.a;
			return 'I can\'t find .' + (found.w + (', this record only has these fields:\n\n    ' + A2($elm$core$String$join, '\n    ', found.jo)));
		case 8:
			var attempting = inf.a;
			return 'You\'re trying to access\n\n    .' + (attempting.w + ('\n\nbut this value isn\'t a record. It\'s a\n\n    ' + $stil4m$elm_syntax$Elm$Writer$write(
				$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(attempting.aa)))));
		case 9:
			var attempting = inf.a;
			return 'You\'re trying to access\n\n    .' + (attempting.w + ('\n\nbut this value isn\'t a record, it\'s a\n\n    ' + ($stil4m$elm_syntax$Elm$Writer$write(
				$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(attempting.aa))) + '\n\nIs this value supposed to be an alias for a record? If so, check out Elm.alias!')));
		case 10:
			var details = inf.a;
			return details.jh + ' not found, though I was trying to unpack it in a let expression.';
		case 12:
			var type_ = inf.a;
			return $stil4m$elm_syntax$Elm$Writer$write(
				$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + ' is not appendable.  Only Strings and Lists are appendable';
		case 13:
			var type_ = inf.a;
			return $stil4m$elm_syntax$Elm$Writer$write(
				$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + ' is not appendable.  Only Strings and Lists are appendable';
		case 14:
			var one = inf.a;
			var two = inf.b;
			return 'I found\n\n    ' + ($stil4m$elm_syntax$Elm$Writer$write(
				$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(one))) + ('\n\nBut I was expecting:\n\n    ' + $stil4m$elm_syntax$Elm$Writer$write(
				$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(two)))));
		default:
			return 'Different lists of type variables';
	}
};
var $mdgriffith$elm_codegen$Elm$renderError = function (err) {
	if (!err.b) {
		return '';
	} else {
		return A2(
			$elm$core$String$join,
			'\n\n',
			A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$inferenceErrorToString, err));
	}
};
var $mdgriffith$elm_codegen$Internal$Compiler$isAppendable = function (annotation) {
	_v0$2:
	while (true) {
		if ((annotation.$ === 1) && (!annotation.a.b.a.b)) {
			switch (annotation.a.b.b) {
				case 'String':
					var _v1 = annotation.a;
					var _v2 = _v1.b;
					return true;
				case 'List':
					if (annotation.b.b && (!annotation.b.b.b)) {
						var _v3 = annotation.a;
						var _v4 = _v3.b;
						var _v5 = annotation.b;
						var _v6 = _v5.a;
						var inner = _v6.b;
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
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$denode = $stil4m$elm_syntax$Elm$Syntax$Node$value;
var $mdgriffith$elm_codegen$Internal$Compiler$isComparable = function (annotation) {
	isComparable:
	while (true) {
		_v0$6:
		while (true) {
			switch (annotation.$) {
				case 1:
					if (annotation.a.b.a.b) {
						if (((annotation.a.b.a.a === 'Char') && (!annotation.a.b.a.b.b)) && (annotation.a.b.b === 'Char')) {
							var _v5 = annotation.a;
							var _v6 = _v5.b;
							var _v7 = _v6.a;
							return true;
						} else {
							break _v0$6;
						}
					} else {
						switch (annotation.a.b.b) {
							case 'Int':
								var _v1 = annotation.a;
								var _v2 = _v1.b;
								return true;
							case 'Float':
								var _v3 = annotation.a;
								var _v4 = _v3.b;
								return true;
							case 'String':
								var _v8 = annotation.a;
								var _v9 = _v8.b;
								return true;
							case 'List':
								if (annotation.b.b && (!annotation.b.b.b)) {
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
					return A2(
						$elm$core$List$all,
						A2($elm$core$Basics$composeL, $mdgriffith$elm_codegen$Internal$Compiler$isComparable, $mdgriffith$elm_codegen$Internal$Compiler$denode),
						innerList);
				default:
					break _v0$6;
			}
		}
		return false;
	}
};
var $mdgriffith$elm_codegen$Internal$Compiler$isNumber = function (annotation) {
	_v0$2:
	while (true) {
		if ((annotation.$ === 1) && (!annotation.a.b.a.b)) {
			switch (annotation.a.b.b) {
				case 'Int':
					var _v1 = annotation.a;
					var _v2 = _v1.b;
					return true;
				case 'Float':
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
var $mdgriffith$elm_codegen$Internal$Compiler$checkRestrictions = F2(
	function (restrictions, type_) {
		switch (restrictions.$) {
			case 0:
				return $elm$core$Result$Ok(type_);
			case 5:
				var constraints = restrictions.a;
				return $elm$core$Result$Err(
					$stil4m$elm_syntax$Elm$Writer$write(
						$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + (' needs to be: ' + (A2(
						$elm$core$String$join,
						', ',
						A2(
							$elm$core$List$concatMap,
							function (constraint) {
								switch (constraint.$) {
									case 0:
										return _List_Nil;
									case 5:
										return _List_Nil;
									case 1:
										return _List_fromArray(
											['a number']);
									case 3:
										return _List_fromArray(
											['comparable']);
									case 2:
										return _List_fromArray(
											['appendable']);
									default:
										return _List_fromArray(
											['appendable and comparable']);
								}
							},
							constraints)) + '\n\nbut that\'s impossible!  Or Elm Codegen\'s s typechecker is off.')));
			case 1:
				return $mdgriffith$elm_codegen$Internal$Compiler$isNumber(type_) ? $elm$core$Result$Ok(type_) : $elm$core$Result$Err(
					$stil4m$elm_syntax$Elm$Writer$write(
						$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + ' is not a number');
			case 3:
				return $mdgriffith$elm_codegen$Internal$Compiler$isComparable(type_) ? $elm$core$Result$Ok(type_) : $elm$core$Result$Err(
					$stil4m$elm_syntax$Elm$Writer$write(
						$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + ' is not comparable.  Only Ints, Floats, Chars, Strings and Lists and Tuples of those things are comparable.');
			case 2:
				return $mdgriffith$elm_codegen$Internal$Compiler$isAppendable(type_) ? $elm$core$Result$Ok(type_) : $elm$core$Result$Err(
					$stil4m$elm_syntax$Elm$Writer$write(
						$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + ' is not appendable.  Only Strings and Lists are appendable.');
			default:
				return ($mdgriffith$elm_codegen$Internal$Compiler$isComparable(type_) || $mdgriffith$elm_codegen$Internal$Compiler$isAppendable(type_)) ? $elm$core$Result$Ok(type_) : $elm$core$Result$Err(
					$stil4m$elm_syntax$Elm$Writer$write(
						$stil4m$elm_syntax$Elm$Writer$writeTypeAnnotation(
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(type_))) + ' is not appendable/comparable.  Only Strings and Lists are allowed here.');
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$NoRestrictions = {$: 0};
var $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit = {$: 2};
var $mdgriffith$elm_codegen$Internal$Compiler$IsAppendable = {$: 2};
var $mdgriffith$elm_codegen$Internal$Compiler$IsAppendableComparable = {$: 4};
var $mdgriffith$elm_codegen$Internal$Compiler$IsComparable = {$: 3};
var $mdgriffith$elm_codegen$Internal$Compiler$IsNumber = {$: 1};
var $elm$core$String$startsWith = _String_startsWith;
var $mdgriffith$elm_codegen$Internal$Compiler$nameToRestrictions = function (name) {
	return A2($elm$core$String$startsWith, 'number', name) ? $mdgriffith$elm_codegen$Internal$Compiler$IsNumber : (A2($elm$core$String$startsWith, 'comparable', name) ? $mdgriffith$elm_codegen$Internal$Compiler$IsComparable : (A2($elm$core$String$startsWith, 'appendable', name) ? $mdgriffith$elm_codegen$Internal$Compiler$IsAppendable : (A2($elm$core$String$startsWith, 'compappend', name) ? $mdgriffith$elm_codegen$Internal$Compiler$IsAppendableComparable : $mdgriffith$elm_codegen$Internal$Compiler$NoRestrictions)));
};
var $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$restrictFurther = F2(
	function (restriction, newRestriction) {
		switch (restriction.$) {
			case 0:
				return newRestriction;
			case 5:
				var constraints = restriction.a;
				switch (newRestriction.$) {
					case 5:
						var newConstraints = newRestriction.a;
						return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(
							_Utils_ap(constraints, newConstraints));
					case 0:
						return restriction;
					default:
						return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(
							A2($elm$core$List$cons, newRestriction, constraints));
				}
			case 1:
				switch (newRestriction.$) {
					case 1:
						return newRestriction;
					case 0:
						return restriction;
					case 5:
						var constraints = newRestriction.a;
						return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(
							A2($elm$core$List$cons, restriction, constraints));
					default:
						return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(
							_List_fromArray(
								[restriction, newRestriction]));
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
						return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(
							A2($elm$core$List$cons, restriction, constraints));
					default:
						return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(
							_List_fromArray(
								[restriction, newRestriction]));
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
						return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(
							A2($elm$core$List$cons, restriction, constraints));
					default:
						return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(
							_List_fromArray(
								[restriction, newRestriction]));
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
						return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(
							A2($elm$core$List$cons, restriction, constraints));
					default:
						return $mdgriffith$elm_codegen$Internal$Compiler$Overconstrainted(
							_List_fromArray(
								[restriction, newRestriction]));
				}
		}
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$getRestrictionsHelper = F3(
	function (existingRestrictions, notation, cache) {
		getRestrictionsHelper:
		while (true) {
			switch (notation.$) {
				case 6:
					var _v1 = notation.a;
					var oneCoords = _v1.a;
					var one = _v1.b;
					var _v2 = notation.b;
					var twoCoords = _v2.a;
					var two = _v2.b;
					return existingRestrictions;
				case 0:
					var name = notation.a;
					var $temp$existingRestrictions = A2(
						$mdgriffith$elm_codegen$Internal$Compiler$restrictFurther,
						existingRestrictions,
						$mdgriffith$elm_codegen$Internal$Compiler$nameToRestrictions(name)),
						$temp$notation = A2(
						$elm$core$Maybe$withDefault,
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit,
						A2($elm$core$Dict$get, name, cache)),
						$temp$cache = cache;
					existingRestrictions = $temp$existingRestrictions;
					notation = $temp$notation;
					cache = $temp$cache;
					continue getRestrictionsHelper;
				case 1:
					var nodedModuleName = notation.a;
					var vars = notation.b;
					return existingRestrictions;
				case 2:
					return existingRestrictions;
				case 3:
					var nodes = notation.a;
					return existingRestrictions;
				case 4:
					var fields = notation.a;
					return existingRestrictions;
				default:
					var baseName = notation.a;
					var _v3 = notation.b;
					var recordNode = _v3.a;
					var fields = _v3.b;
					return existingRestrictions;
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$getRestrictions = F2(
	function (notation, cache) {
		return A3($mdgriffith$elm_codegen$Internal$Compiler$getRestrictionsHelper, $mdgriffith$elm_codegen$Internal$Compiler$NoRestrictions, notation, cache);
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm$core$Result$map2 = F3(
	function (func, ra, rb) {
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
				return $elm$core$Result$Ok(
					A2(func, a, b));
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$resolveVariableList = F4(
	function (visited, cache, nodes, processed) {
		resolveVariableList:
		while (true) {
			if (!nodes.b) {
				return $elm$core$Result$Ok(
					$elm$core$List$reverse(processed));
			} else {
				var _v17 = nodes.a;
				var coords = _v17.a;
				var top = _v17.b;
				var remain = nodes.b;
				var _v18 = A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, visited, cache, top);
				if (!_v18.$) {
					var resolved = _v18.a;
					var $temp$visited = visited,
						$temp$cache = cache,
						$temp$nodes = remain,
						$temp$processed = A2(
						$elm$core$List$cons,
						A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, coords, resolved),
						processed);
					visited = $temp$visited;
					cache = $temp$cache;
					nodes = $temp$nodes;
					processed = $temp$processed;
					continue resolveVariableList;
				} else {
					var err = _v18.a;
					return $elm$core$Result$Err(err);
				}
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$resolveVariables = F3(
	function (visited, cache, annotation) {
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
					return A3(
						$elm$core$Result$map2,
						F2(
							function (oneResolved, twoResolved) {
								return A2(
									$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
									A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, oneCoords, oneResolved),
									A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, twoCoords, twoResolved));
							}),
						A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, visited, cache, one),
						A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, visited, cache, two));
				case 0:
					var name = annotation.a;
					if (A2($elm$core$Set$member, name, visited)) {
						return $elm$core$Result$Err('Infinite type inference loop!  Whoops.  This is an issue with elm-codegen.  If you can report this to the elm-codegen repo, that would be appreciated!');
					} else {
						var _v3 = A2($elm$core$Dict$get, name, cache);
						if (_v3.$ === 1) {
							return $elm$core$Result$Ok(annotation);
						} else {
							var newType = _v3.a;
							var $temp$visited = A2($elm$core$Set$insert, name, visited),
								$temp$cache = cache,
								$temp$annotation = newType;
							visited = $temp$visited;
							cache = $temp$cache;
							annotation = $temp$annotation;
							continue resolveVariables;
						}
					}
				case 1:
					var nodedModuleName = annotation.a;
					var vars = annotation.b;
					return A2(
						$elm$core$Result$map,
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed(nodedModuleName),
						A4($mdgriffith$elm_codegen$Internal$Compiler$resolveVariableList, visited, cache, vars, _List_Nil));
				case 2:
					return $elm$core$Result$Ok($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit);
				case 3:
					var nodes = annotation.a;
					return A2(
						$elm$core$Result$map,
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled,
						A4($mdgriffith$elm_codegen$Internal$Compiler$resolveVariableList, visited, cache, nodes, _List_Nil));
				case 4:
					var fields = annotation.a;
					return A2(
						$elm$core$Result$map,
						A2($elm$core$Basics$composeL, $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record, $elm$core$List$reverse),
						A3(
							$elm$core$List$foldl,
							F2(
								function (_v4, found) {
									var fieldRange = _v4.a;
									var _v5 = _v4.b;
									var name = _v5.a;
									var _v6 = _v5.b;
									var fieldTypeRange = _v6.a;
									var fieldType = _v6.b;
									if (found.$ === 1) {
										var err = found.a;
										return $elm$core$Result$Err(err);
									} else {
										var processedFields = found.a;
										var _v8 = A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, visited, cache, fieldType);
										if (_v8.$ === 1) {
											var err = _v8.a;
											return $elm$core$Result$Err(err);
										} else {
											var resolvedField = _v8.a;
											var restrictions = A2($mdgriffith$elm_codegen$Internal$Compiler$getRestrictions, annotation, cache);
											var _v9 = A2($mdgriffith$elm_codegen$Internal$Compiler$checkRestrictions, restrictions, resolvedField);
											if (!_v9.$) {
												return $elm$core$Result$Ok(
													A2(
														$elm$core$List$cons,
														A2(
															$stil4m$elm_syntax$Elm$Syntax$Node$Node,
															fieldRange,
															_Utils_Tuple2(
																name,
																A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fieldTypeRange, resolvedField))),
														processedFields));
											} else {
												var err = _v9.a;
												return $elm$core$Result$Err(err);
											}
										}
									}
								}),
							$elm$core$Result$Ok(_List_Nil),
							fields));
				default:
					var baseName = annotation.a;
					var _v10 = annotation.b;
					var recordNode = _v10.a;
					var fields = _v10.b;
					var newFieldResult = A3(
						$elm$core$List$foldl,
						F2(
							function (_v11, found) {
								var fieldRange = _v11.a;
								var _v12 = _v11.b;
								var name = _v12.a;
								var _v13 = _v12.b;
								var fieldTypeRange = _v13.a;
								var fieldType = _v13.b;
								if (found.$ === 1) {
									var err = found.a;
									return $elm$core$Result$Err(err);
								} else {
									var processedFields = found.a;
									var _v15 = A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, visited, cache, fieldType);
									if (_v15.$ === 1) {
										var err = _v15.a;
										return $elm$core$Result$Err(err);
									} else {
										var resolvedField = _v15.a;
										var restrictions = A2($mdgriffith$elm_codegen$Internal$Compiler$getRestrictions, annotation, cache);
										return $elm$core$Result$Ok(
											A2(
												$elm$core$List$cons,
												A2(
													$stil4m$elm_syntax$Elm$Syntax$Node$Node,
													fieldRange,
													_Utils_Tuple2(
														name,
														A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, fieldTypeRange, resolvedField))),
												processedFields));
									}
								}
							}),
						$elm$core$Result$Ok(_List_Nil),
						fields);
					return A2(
						$elm$core$Result$map,
						function (newFields) {
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord,
								baseName,
								A2(
									$stil4m$elm_syntax$Elm$Syntax$Node$Node,
									recordNode,
									$elm$core$List$reverse(newFields)));
						},
						newFieldResult);
			}
		}
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper = function (ann) {
	switch (ann.$) {
		case 0:
			var str = ann.a;
			return _List_fromArray(
				[str]);
		case 1:
			var modName = ann.a;
			var anns = ann.b;
			return A2(
				$elm$core$List$concatMap,
				A2($elm$core$Basics$composeL, $mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper, $mdgriffith$elm_codegen$Internal$Compiler$denode),
				anns);
		case 2:
			return _List_Nil;
		case 3:
			var tupled = ann.a;
			return A2(
				$elm$core$List$concatMap,
				A2($elm$core$Basics$composeL, $mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper, $mdgriffith$elm_codegen$Internal$Compiler$denode),
				tupled);
		case 4:
			var recordDefinition = ann.a;
			return A2(
				$elm$core$List$concatMap,
				function (nodedField) {
					var _v1 = $mdgriffith$elm_codegen$Internal$Compiler$denode(nodedField);
					var name = _v1.a;
					var field = _v1.b;
					return $mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper(
						$mdgriffith$elm_codegen$Internal$Compiler$denode(field));
				},
				recordDefinition);
		case 5:
			var recordName = ann.a;
			var recordDefinition = ann.b;
			return A2(
				$elm$core$List$concatMap,
				function (nodedField) {
					var _v2 = $mdgriffith$elm_codegen$Internal$Compiler$denode(nodedField);
					var name = _v2.a;
					var field = _v2.b;
					return $mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper(
						$mdgriffith$elm_codegen$Internal$Compiler$denode(field));
				},
				$mdgriffith$elm_codegen$Internal$Compiler$denode(recordDefinition));
		default:
			var one = ann.a;
			var two = ann.b;
			return A2(
				$elm$core$List$concatMap,
				$mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper,
				_List_fromArray(
					[
						$mdgriffith$elm_codegen$Internal$Compiler$denode(one),
						$mdgriffith$elm_codegen$Internal$Compiler$denode(two)
					]));
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $mdgriffith$elm_codegen$Internal$Compiler$simplify = function (fullStr) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (piece, str) {
				var isDigit = A2($elm$core$String$all, $elm$core$Char$isDigit, piece);
				if (isDigit) {
					return str;
				} else {
					if (str === '') {
						return piece;
					} else {
						return str + ('_' + piece);
					}
				}
			}),
		'',
		A2($elm$core$String$split, '_', fullStr));
};
var $mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariablesHelper = F3(
	function (existing, renames, type_) {
		switch (type_.$) {
			case 0:
				var varName = type_.a;
				var _v1 = A2($elm$core$Dict$get, varName, renames);
				if (_v1.$ === 1) {
					var simplified = $mdgriffith$elm_codegen$Internal$Compiler$simplify(varName);
					return (A2($elm$core$Set$member, simplified, existing) && (!_Utils_eq(varName, simplified))) ? _Utils_Tuple2(
						renames,
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(simplified)) : _Utils_Tuple2(
						A3($elm$core$Dict$insert, varName, simplified, renames),
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(simplified));
				} else {
					var rename = _v1.a;
					return _Utils_Tuple2(
						renames,
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(rename));
				}
			case 1:
				var name = type_.a;
				var vars = type_.b;
				var _v2 = A3(
					$elm$core$List$foldl,
					F2(
						function (typevar, _v3) {
							var varUsed = _v3.a;
							var varList = _v3.b;
							var _v4 = A3(
								$mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariablesHelper,
								existing,
								varUsed,
								$mdgriffith$elm_codegen$Internal$Compiler$denode(typevar));
							var oneUsed = _v4.a;
							var oneType = _v4.b;
							return _Utils_Tuple2(
								oneUsed,
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_codegen$Internal$Compiler$nodify(oneType),
									varList));
						}),
					_Utils_Tuple2(renames, _List_Nil),
					vars);
				var newUsed = _v2.a;
				var newVars = _v2.b;
				return _Utils_Tuple2(
					newUsed,
					A2(
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
						name,
						$elm$core$List$reverse(newVars)));
			case 2:
				return _Utils_Tuple2(renames, type_);
			case 3:
				var valsA = type_.a;
				return _Utils_Tuple2(renames, type_);
			case 4:
				var fieldsA = type_.a;
				return _Utils_Tuple2(renames, type_);
			case 5:
				var _v5 = type_.a;
				var reVarName = _v5.b;
				var _v6 = type_.b;
				var fieldsARange = _v6.a;
				var fieldsA = _v6.b;
				return _Utils_Tuple2(renames, type_);
			default:
				var one = type_.a;
				var two = type_.b;
				var _v7 = A3(
					$mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariablesHelper,
					existing,
					renames,
					$mdgriffith$elm_codegen$Internal$Compiler$denode(one));
				var oneUsed = _v7.a;
				var oneType = _v7.b;
				var _v8 = A3(
					$mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariablesHelper,
					existing,
					oneUsed,
					$mdgriffith$elm_codegen$Internal$Compiler$denode(two));
				var twoUsed = _v8.a;
				var twoType = _v8.b;
				return _Utils_Tuple2(
					twoUsed,
					A2(
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
						$mdgriffith$elm_codegen$Internal$Compiler$nodify(oneType),
						$mdgriffith$elm_codegen$Internal$Compiler$nodify(twoType)));
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariables = function (type_) {
	var existing = $elm$core$Set$fromList(
		$mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper(type_));
	return A3($mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariablesHelper, existing, $elm$core$Dict$empty, type_).b;
};
var $mdgriffith$elm_codegen$Internal$Index$typecheck = function (_v0) {
	var top = _v0.a;
	var tail = _v0.b;
	var scope = _v0.c;
	var check = _v0.d;
	return check;
};
var $mdgriffith$elm_codegen$Internal$Compiler$resolve = F3(
	function (index, cache, annotation) {
		if ($mdgriffith$elm_codegen$Internal$Index$typecheck(index)) {
			var restrictions = A2($mdgriffith$elm_codegen$Internal$Compiler$getRestrictions, annotation, cache);
			var _v0 = A3($mdgriffith$elm_codegen$Internal$Compiler$resolveVariables, $elm$core$Set$empty, cache, annotation);
			if (!_v0.$) {
				var newAnnotation = _v0.a;
				return A2(
					$mdgriffith$elm_codegen$Internal$Compiler$checkRestrictions,
					restrictions,
					$mdgriffith$elm_codegen$Internal$Compiler$rewriteTypeVariables(newAnnotation));
			} else {
				var err = _v0.a;
				return $elm$core$Result$Err(err);
			}
		} else {
			return $elm$core$Result$Err('Type inference skipped.');
		}
	});
var $mdgriffith$elm_codegen$Elm$declaration = F2(
	function (nameStr, _v0) {
		var toBody = _v0;
		var name = $mdgriffith$elm_codegen$Internal$Format$formatDeclarationName(nameStr);
		return $mdgriffith$elm_codegen$Internal$Compiler$Declaration(
			{
				aT: $elm$core$Maybe$Nothing,
				dh: $mdgriffith$elm_codegen$Internal$Compiler$NotExposed,
				a: _List_Nil,
				ao: name,
				ac: function (index) {
					var body = toBody(index);
					var resolvedType = A2(
						$elm$core$Result$andThen,
						function (sig) {
							return A3($mdgriffith$elm_codegen$Internal$Compiler$resolve, index, sig.b, sig.h$);
						},
						A2($elm$core$Result$mapError, $mdgriffith$elm_codegen$Elm$renderError, body.bx));
					var maybeWarning = function () {
						if (!resolvedType.$) {
							var sig = resolvedType.a;
							var _v5 = body.bx;
							if (!_v5.$) {
								var inference = _v5.a;
								return $elm$core$Maybe$Nothing;
							} else {
								if (!_v5.a.b) {
									return $elm$core$Maybe$Nothing;
								} else {
									var err = _v5.a;
									return $elm$core$Maybe$Just(
										{
											jf: name,
											k0: $mdgriffith$elm_codegen$Elm$renderError(err)
										});
								}
							}
						} else {
							if (resolvedType.a === '') {
								return $elm$core$Maybe$Nothing;
							} else {
								var err = resolvedType.a;
								return $elm$core$Maybe$Just(
									{jf: name, k0: err});
							}
						}
					}();
					return {
						T: body.a,
						jf: $stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration(
							{
								jf: function () {
									var _v1 = body.jr;
									if (_v1.$ === 17) {
										var lam = _v1.a;
										return $mdgriffith$elm_codegen$Internal$Compiler$nodify(
											{
												aQ: lam.B,
												jr: lam.jr,
												ao: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name)
											});
									} else {
										return $mdgriffith$elm_codegen$Internal$Compiler$nodify(
											{
												aQ: _List_Nil,
												jr: $mdgriffith$elm_codegen$Internal$Compiler$nodify(body.jr),
												ao: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name)
											});
									}
								}(),
								aU: $elm$core$Maybe$Nothing,
								kE: function () {
									var _v2 = body.bx;
									if (!_v2.$) {
										var sig = _v2.a;
										if (!resolvedType.$) {
											if (!resolvedType.a.$) {
												var generic = resolvedType.a.a;
												return $elm$core$Maybe$Nothing;
											} else {
												var finalType = resolvedType.a;
												return $elm$core$Maybe$Just(
													$mdgriffith$elm_codegen$Internal$Compiler$nodify(
														{
															ao: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name),
															aN: $mdgriffith$elm_codegen$Internal$Compiler$nodify(
																$mdgriffith$elm_codegen$Internal$Clean$clean(finalType))
														}));
											}
										} else {
											var errMsg = resolvedType.a;
											return $elm$core$Maybe$Nothing;
										}
									} else {
										return $elm$core$Maybe$Nothing;
									}
								}()
							}),
						k0: maybeWarning
					};
				}
			});
	});
var $stil4m$elm_syntax$Elm$Syntax$Exposing$All = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_codegen$Internal$Comments$Markdown = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Module$NormalModule = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Module$PortModule = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$RenderedBlock = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$RenderedComment = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$RenderedDecl = function (a) {
	return {$: 0, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration = function (a) {
	return {$: 1, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_codegen$Internal$Render$addDocs = F2(
	function (maybeDoc, decl) {
		if (maybeDoc.$ === 1) {
			return decl;
		} else {
			var doc = maybeDoc.a;
			switch (decl.$) {
				case 0:
					var func = decl.a;
					return $stil4m$elm_syntax$Elm$Syntax$Declaration$FunctionDeclaration(
						_Utils_update(
							func,
							{
								aU: $elm$core$Maybe$Just(
									$mdgriffith$elm_codegen$Internal$Compiler$nodify(doc))
							}));
				case 1:
					var typealias = decl.a;
					return $stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration(
						_Utils_update(
							typealias,
							{
								aU: $elm$core$Maybe$Just(
									$mdgriffith$elm_codegen$Internal$Compiler$nodify(doc))
							}));
				case 2:
					var typeDecl = decl.a;
					return $stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration(
						_Utils_update(
							typeDecl,
							{
								aU: $elm$core$Maybe$Just(
									$mdgriffith$elm_codegen$Internal$Compiler$nodify(doc))
							}));
				case 3:
					var sig = decl.a;
					return decl;
				case 4:
					return decl;
				default:
					return decl;
			}
		}
	});
var $stil4m$elm_syntax$Elm$Syntax$Exposing$FunctionExpose = function (a) {
	return {$: 1, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose = function (a) {
	return {$: 3, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_codegen$Internal$Render$addExposed = F3(
	function (exposed, declaration, otherExposes) {
		if (!exposed.$) {
			return otherExposes;
		} else {
			var details = exposed.a;
			switch (declaration.$) {
				case 0:
					var fn = declaration.a;
					var fnName = $mdgriffith$elm_codegen$Internal$Compiler$denode(
						function ($) {
							return $.ao;
						}(
							$mdgriffith$elm_codegen$Internal$Compiler$denode(fn.jf)));
					return A2(
						$elm$core$List$cons,
						$stil4m$elm_syntax$Elm$Syntax$Exposing$FunctionExpose(fnName),
						otherExposes);
				case 1:
					var synonym = declaration.a;
					var aliasName = $mdgriffith$elm_codegen$Internal$Compiler$denode(synonym.ao);
					return A2(
						$elm$core$List$cons,
						$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose(aliasName),
						otherExposes);
				case 2:
					var myType = declaration.a;
					var typeName = $mdgriffith$elm_codegen$Internal$Compiler$denode(myType.ao);
					return details.jq ? A2(
						$elm$core$List$cons,
						$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeExpose(
							{
								ao: typeName,
								ke: $elm$core$Maybe$Just($stil4m$elm_syntax$Elm$Syntax$Range$emptyRange)
							}),
						otherExposes) : A2(
						$elm$core$List$cons,
						$stil4m$elm_syntax$Elm$Syntax$Exposing$TypeOrAliasExpose(typeName),
						otherExposes);
				case 3:
					var myPort = declaration.a;
					var typeName = $mdgriffith$elm_codegen$Internal$Compiler$denode(myPort.ao);
					return A2(
						$elm$core$List$cons,
						$stil4m$elm_syntax$Elm$Syntax$Exposing$FunctionExpose(typeName),
						otherExposes);
				case 4:
					var inf = declaration.a;
					return otherExposes;
				default:
					return otherExposes;
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Comments$Comment = $elm$core$Basics$identity;
var $mdgriffith$elm_codegen$Internal$Comments$addPart = F2(
	function (_v0, part) {
		var parts = _v0;
		return A2($elm$core$List$cons, part, parts);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$fullModName = function (name) {
	return A2($elm$core$String$join, '.', name);
};
var $elm$core$List$sortBy = _List_sortBy;
var $mdgriffith$elm_codegen$Internal$Render$dedupImports = function (mods) {
	return A2(
		$elm$core$List$sortBy,
		$mdgriffith$elm_codegen$Internal$Compiler$fullModName,
		A3(
			$elm$core$List$foldl,
			F2(
				function (mod, _v0) {
					var set = _v0.a;
					var gathered = _v0.b;
					var stringName = $mdgriffith$elm_codegen$Internal$Compiler$fullModName(mod);
					return A2($elm$core$Set$member, stringName, set) ? _Utils_Tuple2(set, gathered) : _Utils_Tuple2(
						A2($elm$core$Set$insert, stringName, set),
						A2($elm$core$List$cons, mod, gathered));
				}),
			_Utils_Tuple2($elm$core$Set$empty, _List_Nil),
			mods).b);
};
var $mdgriffith$elm_codegen$Internal$Comments$emptyComment = _List_Nil;
var $mdgriffith$elm_codegen$Internal$Render$matchName = F2(
	function (one, two) {
		if (one.$ === 1) {
			if (two.$ === 1) {
				return true;
			} else {
				return false;
			}
		} else {
			var oneName = one.a;
			if (two.$ === 1) {
				return false;
			} else {
				var twoName = two.a;
				return _Utils_eq(oneName, twoName);
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Render$groupExposing = function (items) {
	return A3(
		$elm$core$List$foldr,
		F2(
			function (_v0, acc) {
				var maybeGroup = _v0.a;
				var name = _v0.b;
				if (!acc.b) {
					return _List_fromArray(
						[
							{
							dV: maybeGroup,
							j0: _List_fromArray(
								[name])
						}
						]);
				} else {
					var top = acc.a;
					var groups = acc.b;
					return A2($mdgriffith$elm_codegen$Internal$Render$matchName, maybeGroup, top.dV) ? A2(
						$elm$core$List$cons,
						{
							dV: top.dV,
							j0: A2($elm$core$List$cons, name, top.j0)
						},
						groups) : A2(
						$elm$core$List$cons,
						{
							dV: maybeGroup,
							j0: _List_fromArray(
								[name])
						},
						acc);
				}
			}),
		_List_Nil,
		items);
};
var $mdgriffith$elm_codegen$Internal$Compiler$builtIn = function (name) {
	_v0$4:
	while (true) {
		if (name.b && (!name.b.b)) {
			switch (name.a) {
				case 'List':
					return true;
				case 'Maybe':
					return true;
				case 'String':
					return true;
				case 'Basics':
					return true;
				default:
					break _v0$4;
			}
		} else {
			break _v0$4;
		}
	}
	return false;
};
var $mdgriffith$elm_codegen$Internal$Compiler$findAlias = F2(
	function (modName, aliases) {
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
					var $temp$modName = modName,
						$temp$aliases = remain;
					modName = $temp$modName;
					aliases = $temp$aliases;
					continue findAlias;
				}
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$makeImport = F2(
	function (aliases, name) {
		if (!name.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$findAlias, name, aliases);
			if (_v1.$ === 1) {
				return $mdgriffith$elm_codegen$Internal$Compiler$builtIn(name) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
					{
						di: $elm$core$Maybe$Nothing,
						fg: $elm$core$Maybe$Nothing,
						a1: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name)
					});
			} else {
				var alias = _v1.a;
				return $elm$core$Maybe$Just(
					{
						di: $elm$core$Maybe$Nothing,
						fg: $elm$core$Maybe$Just(
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(
								_List_fromArray(
									[alias]))),
						a1: $mdgriffith$elm_codegen$Internal$Compiler$nodify(name)
					});
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$nodifyAll = $elm$core$List$map($mdgriffith$elm_codegen$Internal$Compiler$nodify);
var $the_sett$elm_pretty_printer$Internals$Concatenate = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $the_sett$elm_pretty_printer$Pretty$append = F2(
	function (doc1, doc2) {
		return A2(
			$the_sett$elm_pretty_printer$Internals$Concatenate,
			function (_v0) {
				return doc1;
			},
			function (_v1) {
				return doc2;
			});
	});
var $elm_community$basics_extra$Basics$Extra$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var $the_sett$elm_pretty_printer$Pretty$a = $elm_community$basics_extra$Basics$Extra$flip($the_sett$elm_pretty_printer$Pretty$append);
var $the_sett$elm_pretty_printer$Internals$Line = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $the_sett$elm_pretty_printer$Pretty$line = A2($the_sett$elm_pretty_printer$Internals$Line, ' ', '');
var $the_sett$elm_pretty_printer$Internals$Empty = {$: 0};
var $the_sett$elm_pretty_printer$Pretty$empty = $the_sett$elm_pretty_printer$Internals$Empty;
var $the_sett$elm_pretty_printer$Pretty$join = F2(
	function (sep, docs) {
		join:
		while (true) {
			if (!docs.b) {
				return $the_sett$elm_pretty_printer$Pretty$empty;
			} else {
				if (!docs.a.$) {
					var _v1 = docs.a;
					var ds = docs.b;
					var $temp$sep = sep,
						$temp$docs = ds;
					sep = $temp$sep;
					docs = $temp$docs;
					continue join;
				} else {
					var d = docs.a;
					var ds = docs.b;
					var step = F2(
						function (x, rest) {
							if (!x.$) {
								return rest;
							} else {
								var doc = x;
								return A2(
									$the_sett$elm_pretty_printer$Pretty$append,
									sep,
									A2($the_sett$elm_pretty_printer$Pretty$append, doc, rest));
							}
						});
					var spersed = A3($elm$core$List$foldr, step, $the_sett$elm_pretty_printer$Pretty$empty, ds);
					return A2($the_sett$elm_pretty_printer$Pretty$append, d, spersed);
				}
			}
		}
	});
var $the_sett$elm_pretty_printer$Pretty$lines = $the_sett$elm_pretty_printer$Pretty$join($the_sett$elm_pretty_printer$Pretty$line);
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe = $elm$core$Maybe$map($mdgriffith$elm_codegen$Internal$Compiler$denode);
var $mdgriffith$elm_codegen$Internal$Compiler$denodeAll = $elm$core$List$map($mdgriffith$elm_codegen$Internal$Compiler$denode);
var $the_sett$elm_pretty_printer$Internals$Text = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $the_sett$elm_pretty_printer$Pretty$char = function (c) {
	return A2(
		$the_sett$elm_pretty_printer$Internals$Text,
		$elm$core$String$fromChar(c),
		$elm$core$Maybe$Nothing);
};
var $the_sett$elm_pretty_printer$Pretty$surround = F3(
	function (left, right, doc) {
		return A2(
			$the_sett$elm_pretty_printer$Pretty$append,
			A2($the_sett$elm_pretty_printer$Pretty$append, left, doc),
			right);
	});
var $the_sett$elm_pretty_printer$Pretty$parens = function (doc) {
	return A3(
		$the_sett$elm_pretty_printer$Pretty$surround,
		$the_sett$elm_pretty_printer$Pretty$char('('),
		$the_sett$elm_pretty_printer$Pretty$char(')'),
		doc);
};
var $the_sett$elm_pretty_printer$Pretty$string = function (val) {
	return A2($the_sett$elm_pretty_printer$Internals$Text, val, $elm$core$Maybe$Nothing);
};
var $mdgriffith$elm_codegen$Internal$Write$prettyTopLevelExpose = function (tlExpose) {
	switch (tlExpose.$) {
		case 0:
			var val = tlExpose.a;
			return $the_sett$elm_pretty_printer$Pretty$parens(
				$the_sett$elm_pretty_printer$Pretty$string(val));
		case 1:
			var val = tlExpose.a;
			return $the_sett$elm_pretty_printer$Pretty$string(val);
		case 2:
			var val = tlExpose.a;
			return $the_sett$elm_pretty_printer$Pretty$string(val);
		default:
			var exposedType = tlExpose.a;
			var _v1 = exposedType.ke;
			if (_v1.$ === 1) {
				return $the_sett$elm_pretty_printer$Pretty$string(exposedType.ao);
			} else {
				return A2(
					$the_sett$elm_pretty_printer$Pretty$a,
					$the_sett$elm_pretty_printer$Pretty$string('(..)'),
					$the_sett$elm_pretty_printer$Pretty$string(exposedType.ao));
			}
	}
};
var $mdgriffith$elm_codegen$Internal$Write$prettyTopLevelExposes = function (exposes) {
	return A2(
		$the_sett$elm_pretty_printer$Pretty$join,
		$the_sett$elm_pretty_printer$Pretty$string(', '),
		A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Write$prettyTopLevelExpose, exposes));
};
var $stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$combineTopLevelExposes = function (exposes) {
	if (!exposes.b) {
		return $stil4m$elm_syntax$Elm$Syntax$Exposing$InfixExpose('');
	} else {
		var hd = exposes.a;
		var tl = exposes.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (exp, result) {
					var _v1 = _Utils_Tuple2(exp, result);
					if (_v1.a.$ === 3) {
						var typeExpose = _v1.a.a;
						var _v2 = typeExpose.ke;
						if (!_v2.$) {
							return exp;
						} else {
							return result;
						}
					} else {
						if (_v1.b.$ === 3) {
							var typeExpose = _v1.b.a;
							var _v3 = typeExpose.ke;
							if (!_v3.$) {
								return result;
							} else {
								return exp;
							}
						} else {
							return result;
						}
					}
				}),
			hd,
			tl);
	}
};
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName = function (tle) {
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
			return exposedType.ao;
	}
};
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$groupByExposingName = function (innerImports) {
	var _v0 = function () {
		if (!innerImports.b) {
			return _Utils_Tuple3(
				'',
				_List_Nil,
				_List_fromArray(
					[_List_Nil]));
		} else {
			var hd = innerImports.a;
			return A3(
				$elm$core$List$foldl,
				F2(
					function (exp, _v2) {
						var currName = _v2.a;
						var currAccum = _v2.b;
						var accum = _v2.c;
						var nextName = $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(exp);
						return _Utils_eq(nextName, currName) ? _Utils_Tuple3(
							currName,
							A2($elm$core$List$cons, exp, currAccum),
							accum) : _Utils_Tuple3(
							nextName,
							_List_fromArray(
								[exp]),
							A2($elm$core$List$cons, currAccum, accum));
					}),
				_Utils_Tuple3(
					$mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(hd),
					_List_Nil,
					_List_Nil),
				innerImports);
		}
	}();
	var hdGroup = _v0.b;
	var remGroups = _v0.c;
	return $elm$core$List$reverse(
		A2($elm$core$List$cons, hdGroup, remGroups));
};
var $elm$core$List$sortWith = _List_sortWith;
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeOrder = F2(
	function (tlel, tler) {
		var _v0 = _Utils_Tuple2(tlel, tler);
		if (!_v0.a.$) {
			if (!_v0.b.$) {
				return A2(
					$elm$core$Basics$compare,
					$mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(tlel),
					$mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(tler));
			} else {
				return 0;
			}
		} else {
			if (!_v0.b.$) {
				return 2;
			} else {
				return A2(
					$elm$core$Basics$compare,
					$mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(tlel),
					$mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeName(tler));
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupExposings = function (tlExposings) {
	return A2(
		$elm$core$List$map,
		$mdgriffith$elm_codegen$Internal$ImportsAndExposing$combineTopLevelExposes,
		$mdgriffith$elm_codegen$Internal$ImportsAndExposing$groupByExposingName(
			A2($elm$core$List$sortWith, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$topLevelExposeOrder, tlExposings)));
};
var $the_sett$elm_pretty_printer$Pretty$space = $the_sett$elm_pretty_printer$Pretty$char(' ');
var $mdgriffith$elm_codegen$Internal$Write$prettyExposing = function (exposing_) {
	var exposings = function () {
		if (!exposing_.$) {
			return $the_sett$elm_pretty_printer$Pretty$parens(
				$the_sett$elm_pretty_printer$Pretty$string('..'));
		} else {
			var tll = exposing_.a;
			return $the_sett$elm_pretty_printer$Pretty$parens(
				$mdgriffith$elm_codegen$Internal$Write$prettyTopLevelExposes(
					$mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupExposings(
						$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(tll))));
		}
	}();
	return A2(
		$the_sett$elm_pretty_printer$Pretty$a,
		exposings,
		A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$space,
			$the_sett$elm_pretty_printer$Pretty$string('exposing')));
};
var $mdgriffith$elm_codegen$Internal$Write$prettyMaybe = F2(
	function (prettyFn, maybeVal) {
		return A2(
			$elm$core$Maybe$withDefault,
			$the_sett$elm_pretty_printer$Pretty$empty,
			A2($elm$core$Maybe$map, prettyFn, maybeVal));
	});
var $mdgriffith$elm_codegen$Internal$Write$dot = $the_sett$elm_pretty_printer$Pretty$string('.');
var $mdgriffith$elm_codegen$Internal$Write$prettyModuleName = function (name) {
	return A2(
		$the_sett$elm_pretty_printer$Pretty$join,
		$mdgriffith$elm_codegen$Internal$Write$dot,
		A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, name));
};
var $mdgriffith$elm_codegen$Internal$Write$prettyModuleNameAlias = function (name) {
	if (!name.b) {
		return $the_sett$elm_pretty_printer$Pretty$empty;
	} else {
		return A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			A2(
				$the_sett$elm_pretty_printer$Pretty$join,
				$mdgriffith$elm_codegen$Internal$Write$dot,
				A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, name)),
			$the_sett$elm_pretty_printer$Pretty$string('as '));
	}
};
var $mdgriffith$elm_codegen$Internal$Write$prettyImport = function (import_) {
	return A2(
		$the_sett$elm_pretty_printer$Pretty$join,
		$the_sett$elm_pretty_printer$Pretty$space,
		_List_fromArray(
			[
				$the_sett$elm_pretty_printer$Pretty$string('import'),
				$mdgriffith$elm_codegen$Internal$Write$prettyModuleName(
				$mdgriffith$elm_codegen$Internal$Compiler$denode(import_.a1)),
				A2(
				$mdgriffith$elm_codegen$Internal$Write$prettyMaybe,
				$mdgriffith$elm_codegen$Internal$Write$prettyModuleNameAlias,
				$mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(import_.fg)),
				A2(
				$mdgriffith$elm_codegen$Internal$Write$prettyMaybe,
				$mdgriffith$elm_codegen$Internal$Write$prettyExposing,
				$mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(import_.di))
			]));
};
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode = $stil4m$elm_syntax$Elm$Syntax$Node$value;
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeMaybe = $elm$core$Maybe$map($mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode);
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeAll = $elm$core$List$map($mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode);
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodify = function (exp) {
	return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, exp);
};
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodifyAll = $elm$core$List$map($mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodify);
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$joinExposings = F2(
	function (left, right) {
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
				return $stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
					$mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodifyAll(
						A2(
							$elm$core$List$append,
							$mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeAll(leftNodes),
							$mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeAll(rightNodes))));
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$joinMaybeExposings = F2(
	function (maybeLeft, maybeRight) {
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
				return $elm$core$Maybe$Just(
					A2($mdgriffith$elm_codegen$Internal$ImportsAndExposing$joinExposings, left, right));
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodifyMaybe = $elm$core$Maybe$map($mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodify);
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$or = F2(
	function (ma, mb) {
		if (ma.$ === 1) {
			return mb;
		} else {
			return ma;
		}
	});
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupExposing = function (exp) {
	if (!exp.$) {
		var range = exp.a;
		return $stil4m$elm_syntax$Elm$Syntax$Exposing$All(range);
	} else {
		var nodes = exp.a;
		return $stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
			$mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodifyAll(
				$mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupExposings(
					$mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeAll(nodes))));
	}
};
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$combineImports = function (innerImports) {
	if (!innerImports.b) {
		return {
			di: $elm$core$Maybe$Nothing,
			fg: $elm$core$Maybe$Nothing,
			a1: $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodify(_List_Nil)
		};
	} else {
		var hd = innerImports.a;
		var tl = innerImports.b;
		var combinedImports = A3(
			$elm$core$List$foldl,
			F2(
				function (imp, result) {
					return {
						di: $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodifyMaybe(
							A2(
								$mdgriffith$elm_codegen$Internal$ImportsAndExposing$joinMaybeExposings,
								$mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeMaybe(imp.di),
								$mdgriffith$elm_codegen$Internal$ImportsAndExposing$denodeMaybe(result.di))),
						fg: A2($mdgriffith$elm_codegen$Internal$ImportsAndExposing$or, imp.fg, result.fg),
						a1: imp.a1
					};
				}),
			hd,
			tl);
		return _Utils_update(
			combinedImports,
			{
				di: A2(
					$elm$core$Maybe$map,
					A2(
						$elm$core$Basics$composeR,
						$mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode,
						A2($elm$core$Basics$composeR, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupExposing, $mdgriffith$elm_codegen$Internal$ImportsAndExposing$nodify)),
					combinedImports.di)
			});
	}
};
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$groupByModuleName = function (innerImports) {
	var _v0 = function () {
		if (!innerImports.b) {
			return _Utils_Tuple3(
				_List_Nil,
				_List_Nil,
				_List_fromArray(
					[_List_Nil]));
		} else {
			var hd = innerImports.a;
			return A3(
				$elm$core$List$foldl,
				F2(
					function (imp, _v2) {
						var currName = _v2.a;
						var currAccum = _v2.b;
						var accum = _v2.c;
						var nextName = $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode(imp.a1);
						return _Utils_eq(nextName, currName) ? _Utils_Tuple3(
							currName,
							A2($elm$core$List$cons, imp, currAccum),
							accum) : _Utils_Tuple3(
							nextName,
							_List_fromArray(
								[imp]),
							A2($elm$core$List$cons, currAccum, accum));
					}),
				_Utils_Tuple3(
					$mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode(hd.a1),
					_List_Nil,
					_List_Nil),
				innerImports);
		}
	}();
	var hdGroup = _v0.b;
	var remGroups = _v0.c;
	return $elm$core$List$reverse(
		A2($elm$core$List$cons, hdGroup, remGroups));
};
var $mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupImports = function (imports) {
	var impName = function (imp) {
		return $mdgriffith$elm_codegen$Internal$ImportsAndExposing$denode(imp.a1);
	};
	return A2(
		$elm$core$List$map,
		$mdgriffith$elm_codegen$Internal$ImportsAndExposing$combineImports,
		$mdgriffith$elm_codegen$Internal$ImportsAndExposing$groupByModuleName(
			A2($elm$core$List$sortBy, impName, imports)));
};
var $mdgriffith$elm_codegen$Internal$Write$prettyImports = function (imports) {
	return $the_sett$elm_pretty_printer$Pretty$lines(
		A2(
			$elm$core$List$map,
			$mdgriffith$elm_codegen$Internal$Write$prettyImport,
			$mdgriffith$elm_codegen$Internal$ImportsAndExposing$sortAndDedupImports(imports)));
};
var $mdgriffith$elm_codegen$Internal$Write$importsPretty = function (imports) {
	if (!imports.b) {
		return $the_sett$elm_pretty_printer$Pretty$line;
	} else {
		return A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$line,
			A2(
				$the_sett$elm_pretty_printer$Pretty$a,
				$the_sett$elm_pretty_printer$Pretty$line,
				A2(
					$the_sett$elm_pretty_printer$Pretty$a,
					$the_sett$elm_pretty_printer$Pretty$line,
					$mdgriffith$elm_codegen$Internal$Write$prettyImports(imports))));
	}
};
var $mdgriffith$elm_codegen$Internal$Write$prettyComments = function (comments) {
	if (!comments.b) {
		return $the_sett$elm_pretty_printer$Pretty$empty;
	} else {
		return A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$line,
			A2(
				$the_sett$elm_pretty_printer$Pretty$a,
				$the_sett$elm_pretty_printer$Pretty$line,
				$the_sett$elm_pretty_printer$Pretty$lines(
					A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, comments))));
	}
};
var $the_sett$elm_pretty_printer$Internals$Nest = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $the_sett$elm_pretty_printer$Pretty$nest = F2(
	function (depth, doc) {
		return A2(
			$the_sett$elm_pretty_printer$Internals$Nest,
			depth,
			function (_v0) {
				return doc;
			});
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyDocumentation = function (docs) {
	return A2($elm$core$String$contains, '\n', docs) ? $the_sett$elm_pretty_printer$Pretty$string('{-| ' + (docs + '\n-}')) : $the_sett$elm_pretty_printer$Pretty$string('{-| ' + (docs + ' -}'));
};
var $the_sett$elm_pretty_printer$Internals$Union = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $the_sett$elm_pretty_printer$Internals$flatten = function (doc) {
	flatten:
	while (true) {
		switch (doc.$) {
			case 1:
				var doc1 = doc.a;
				var doc2 = doc.b;
				return A2(
					$the_sett$elm_pretty_printer$Internals$Concatenate,
					function (_v1) {
						return $the_sett$elm_pretty_printer$Internals$flatten(
							doc1(0));
					},
					function (_v2) {
						return $the_sett$elm_pretty_printer$Internals$flatten(
							doc2(0));
					});
			case 2:
				var i = doc.a;
				var doc1 = doc.b;
				return A2(
					$the_sett$elm_pretty_printer$Internals$Nest,
					i,
					function (_v3) {
						return $the_sett$elm_pretty_printer$Internals$flatten(
							doc1(0));
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
var $the_sett$elm_pretty_printer$Pretty$group = function (doc) {
	return A2(
		$the_sett$elm_pretty_printer$Internals$Union,
		$the_sett$elm_pretty_printer$Internals$flatten(doc),
		doc);
};
var $mdgriffith$elm_codegen$Internal$Write$isNakedCompound = function (typeAnn) {
	switch (typeAnn.$) {
		case 1:
			if (!typeAnn.b.b) {
				return false;
			} else {
				var args = typeAnn.b;
				return true;
			}
		case 6:
			return true;
		default:
			return false;
	}
};
var $elm$core$Tuple$mapBoth = F3(
	function (funcA, funcB, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			funcA(x),
			funcB(y));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyModuleNameDot = F2(
	function (aliases, name) {
		if (!name.b) {
			return $the_sett$elm_pretty_printer$Pretty$empty;
		} else {
			var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$findAlias, name, aliases);
			if (_v1.$ === 1) {
				return A2(
					$the_sett$elm_pretty_printer$Pretty$a,
					$mdgriffith$elm_codegen$Internal$Write$dot,
					A2(
						$the_sett$elm_pretty_printer$Pretty$join,
						$mdgriffith$elm_codegen$Internal$Write$dot,
						A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, name)));
			} else {
				var alias = _v1.a;
				return A2(
					$the_sett$elm_pretty_printer$Pretty$a,
					$mdgriffith$elm_codegen$Internal$Write$dot,
					$the_sett$elm_pretty_printer$Pretty$string(alias));
			}
		}
	});
var $the_sett$elm_pretty_printer$Pretty$separators = function (sep) {
	return $the_sett$elm_pretty_printer$Pretty$join(
		A2($the_sett$elm_pretty_printer$Internals$Line, sep, sep));
};
var $the_sett$elm_pretty_printer$Pretty$words = $the_sett$elm_pretty_printer$Pretty$join($the_sett$elm_pretty_printer$Pretty$space);
var $mdgriffith$elm_codegen$Internal$Write$prettyFieldTypeAnn = F2(
	function (aliases, _v8) {
		var name = _v8.a;
		var ann = _v8.b;
		return $the_sett$elm_pretty_printer$Pretty$group(
			A2(
				$the_sett$elm_pretty_printer$Pretty$nest,
				4,
				$the_sett$elm_pretty_printer$Pretty$lines(
					_List_fromArray(
						[
							$the_sett$elm_pretty_printer$Pretty$words(
							_List_fromArray(
								[
									$the_sett$elm_pretty_printer$Pretty$string(name),
									$the_sett$elm_pretty_printer$Pretty$string(':')
								])),
							A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, ann)
						]))));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyFunctionTypeAnnotation = F3(
	function (aliases, left, right) {
		var expandLeft = function (ann) {
			if (ann.$ === 6) {
				return A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotationParens, aliases, ann);
			} else {
				return A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, ann);
			}
		};
		var innerFnTypeAnn = F2(
			function (innerLeft, innerRight) {
				var rightSide = expandRight(
					$mdgriffith$elm_codegen$Internal$Compiler$denode(innerRight));
				if (rightSide.b) {
					var hd = rightSide.a;
					var tl = rightSide.b;
					return A2(
						$elm$core$List$cons,
						expandLeft(
							$mdgriffith$elm_codegen$Internal$Compiler$denode(innerLeft)),
						A2(
							$elm$core$List$cons,
							$the_sett$elm_pretty_printer$Pretty$words(
								_List_fromArray(
									[
										$the_sett$elm_pretty_printer$Pretty$string('->'),
										hd
									])),
							tl));
				} else {
					return _List_Nil;
				}
			});
		var expandRight = function (ann) {
			if (ann.$ === 6) {
				var innerLeft = ann.a;
				var innerRight = ann.b;
				return A2(innerFnTypeAnn, innerLeft, innerRight);
			} else {
				return _List_fromArray(
					[
						A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, ann)
					]);
			}
		};
		return $the_sett$elm_pretty_printer$Pretty$group(
			$the_sett$elm_pretty_printer$Pretty$lines(
				A2(innerFnTypeAnn, left, right)));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyGenericRecord = F3(
	function (aliases, paramName, fields) {
		var open = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$line,
			$the_sett$elm_pretty_printer$Pretty$words(
				_List_fromArray(
					[
						$the_sett$elm_pretty_printer$Pretty$string('{'),
						$the_sett$elm_pretty_printer$Pretty$string(paramName)
					])));
		var close = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$string('}'),
			$the_sett$elm_pretty_printer$Pretty$line);
		var addBarToFirst = function (exprs) {
			if (!exprs.b) {
				return _List_Nil;
			} else {
				var hd = exprs.a;
				var tl = exprs.b;
				return A2(
					$elm$core$List$cons,
					A2(
						$the_sett$elm_pretty_printer$Pretty$a,
						hd,
						$the_sett$elm_pretty_printer$Pretty$string('| ')),
					tl);
			}
		};
		if (!fields.b) {
			return $the_sett$elm_pretty_printer$Pretty$string('{}');
		} else {
			return $the_sett$elm_pretty_printer$Pretty$group(
				A3(
					$the_sett$elm_pretty_printer$Pretty$surround,
					$the_sett$elm_pretty_printer$Pretty$empty,
					close,
					A2(
						$the_sett$elm_pretty_printer$Pretty$nest,
						4,
						A2(
							$the_sett$elm_pretty_printer$Pretty$a,
							A2(
								$the_sett$elm_pretty_printer$Pretty$separators,
								', ',
								addBarToFirst(
									A2(
										$elm$core$List$map,
										$mdgriffith$elm_codegen$Internal$Write$prettyFieldTypeAnn(aliases),
										A2(
											$elm$core$List$map,
											A2($elm$core$Tuple$mapBoth, $mdgriffith$elm_codegen$Internal$Compiler$denode, $mdgriffith$elm_codegen$Internal$Compiler$denode),
											fields)))),
							open))));
		}
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyRecord = F2(
	function (aliases, fields) {
		var open = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$space,
			$the_sett$elm_pretty_printer$Pretty$string('{'));
		var close = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$string('}'),
			$the_sett$elm_pretty_printer$Pretty$line);
		if (!fields.b) {
			return $the_sett$elm_pretty_printer$Pretty$string('{}');
		} else {
			return $the_sett$elm_pretty_printer$Pretty$group(
				A3(
					$the_sett$elm_pretty_printer$Pretty$surround,
					open,
					close,
					A2(
						$the_sett$elm_pretty_printer$Pretty$separators,
						', ',
						A2(
							$elm$core$List$map,
							$mdgriffith$elm_codegen$Internal$Write$prettyFieldTypeAnn(aliases),
							A2(
								$elm$core$List$map,
								A2($elm$core$Tuple$mapBoth, $mdgriffith$elm_codegen$Internal$Compiler$denode, $mdgriffith$elm_codegen$Internal$Compiler$denode),
								fields)))));
		}
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyTupled = F2(
	function (aliases, anns) {
		return $the_sett$elm_pretty_printer$Pretty$parens(
			A2(
				$the_sett$elm_pretty_printer$Pretty$a,
				$the_sett$elm_pretty_printer$Pretty$space,
				A2(
					$the_sett$elm_pretty_printer$Pretty$a,
					A2(
						$the_sett$elm_pretty_printer$Pretty$join,
						$the_sett$elm_pretty_printer$Pretty$string(', '),
						A2(
							$elm$core$List$map,
							$mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation(aliases),
							$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(anns))),
					$the_sett$elm_pretty_printer$Pretty$space)));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation = F2(
	function (aliases, typeAnn) {
		switch (typeAnn.$) {
			case 0:
				var val = typeAnn.a;
				return $the_sett$elm_pretty_printer$Pretty$string(val);
			case 1:
				var fqName = typeAnn.a;
				var anns = typeAnn.b;
				return A3($mdgriffith$elm_codegen$Internal$Write$prettyTyped, aliases, fqName, anns);
			case 2:
				return $the_sett$elm_pretty_printer$Pretty$string('()');
			case 3:
				var anns = typeAnn.a;
				return A2($mdgriffith$elm_codegen$Internal$Write$prettyTupled, aliases, anns);
			case 4:
				var recordDef = typeAnn.a;
				return A2(
					$mdgriffith$elm_codegen$Internal$Write$prettyRecord,
					aliases,
					$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(recordDef));
			case 5:
				var paramName = typeAnn.a;
				var recordDef = typeAnn.b;
				return A3(
					$mdgriffith$elm_codegen$Internal$Write$prettyGenericRecord,
					aliases,
					$mdgriffith$elm_codegen$Internal$Compiler$denode(paramName),
					$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(
						$mdgriffith$elm_codegen$Internal$Compiler$denode(recordDef)));
			default:
				var fromAnn = typeAnn.a;
				var toAnn = typeAnn.b;
				return A3($mdgriffith$elm_codegen$Internal$Write$prettyFunctionTypeAnnotation, aliases, fromAnn, toAnn);
		}
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotationParens = F2(
	function (aliases, typeAnn) {
		return $mdgriffith$elm_codegen$Internal$Write$isNakedCompound(typeAnn) ? $the_sett$elm_pretty_printer$Pretty$parens(
			A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, typeAnn)) : A2($mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation, aliases, typeAnn);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyTyped = F3(
	function (aliases, fqName, anns) {
		var argsDoc = $the_sett$elm_pretty_printer$Pretty$words(
			A2(
				$elm$core$List$map,
				$mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotationParens(aliases),
				$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(anns)));
		var _v0 = $mdgriffith$elm_codegen$Internal$Compiler$denode(fqName);
		var moduleName = _v0.a;
		var typeName = _v0.b;
		var typeDoc = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$string(typeName),
			A2($mdgriffith$elm_codegen$Internal$Write$prettyModuleNameDot, aliases, moduleName));
		return $the_sett$elm_pretty_printer$Pretty$words(
			_List_fromArray(
				[typeDoc, argsDoc]));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyValueConstructor = F2(
	function (aliases, cons) {
		return A2(
			$the_sett$elm_pretty_printer$Pretty$nest,
			4,
			$the_sett$elm_pretty_printer$Pretty$group(
				$the_sett$elm_pretty_printer$Pretty$lines(
					_List_fromArray(
						[
							$the_sett$elm_pretty_printer$Pretty$string(
							$mdgriffith$elm_codegen$Internal$Compiler$denode(cons.ao)),
							$the_sett$elm_pretty_printer$Pretty$lines(
							A2(
								$elm$core$List$map,
								$mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotationParens(aliases),
								$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(cons.aQ)))
						]))));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyValueConstructors = F2(
	function (aliases, constructors) {
		return A2(
			$the_sett$elm_pretty_printer$Pretty$join,
			A2(
				$the_sett$elm_pretty_printer$Pretty$a,
				$the_sett$elm_pretty_printer$Pretty$string('| '),
				$the_sett$elm_pretty_printer$Pretty$line),
			A2(
				$elm$core$List$map,
				$mdgriffith$elm_codegen$Internal$Write$prettyValueConstructor(aliases),
				constructors));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyCustomType = F2(
	function (aliases, type_) {
		var customTypePretty = A2(
			$the_sett$elm_pretty_printer$Pretty$nest,
			4,
			A2(
				$the_sett$elm_pretty_printer$Pretty$a,
				A2(
					$mdgriffith$elm_codegen$Internal$Write$prettyValueConstructors,
					aliases,
					$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(type_.ja)),
				A2(
					$the_sett$elm_pretty_printer$Pretty$a,
					$the_sett$elm_pretty_printer$Pretty$string('= '),
					A2(
						$the_sett$elm_pretty_printer$Pretty$a,
						$the_sett$elm_pretty_printer$Pretty$line,
						$the_sett$elm_pretty_printer$Pretty$words(
							_List_fromArray(
								[
									$the_sett$elm_pretty_printer$Pretty$string('type'),
									$the_sett$elm_pretty_printer$Pretty$string(
									$mdgriffith$elm_codegen$Internal$Compiler$denode(type_.ao)),
									$the_sett$elm_pretty_printer$Pretty$words(
									A2(
										$elm$core$List$map,
										$the_sett$elm_pretty_printer$Pretty$string,
										$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(type_.dP)))
								]))))));
		return $the_sett$elm_pretty_printer$Pretty$lines(
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_codegen$Internal$Write$prettyMaybe,
					$mdgriffith$elm_codegen$Internal$Write$prettyDocumentation,
					$mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(type_.aU)),
					customTypePretty
				]));
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression = function (a) {
	return {$: 14, a: a};
};
var $mdgriffith$elm_codegen$Internal$Write$adjustExpressionParentheses = F2(
	function (context, expression) {
		var shouldRemove = function (expr) {
			var _v3 = _Utils_Tuple3(context.O, context.N, expr);
			_v3$1:
			while (true) {
				if (_v3.a) {
					return true;
				} else {
					switch (_v3.c.$) {
						case 1:
							if (_v3.b) {
								break _v3$1;
							} else {
								return (context.kl < 11) ? true : false;
							}
						case 3:
							if (_v3.b) {
								break _v3$1;
							} else {
								var _v4 = _v3.c;
								return true;
							}
						case 7:
							if (_v3.b) {
								break _v3$1;
							} else {
								return true;
							}
						case 8:
							if (_v3.b) {
								break _v3$1;
							} else {
								return true;
							}
						case 9:
							if (_v3.b) {
								break _v3$1;
							} else {
								return true;
							}
						case 10:
							if (_v3.b) {
								break _v3$1;
							} else {
								return true;
							}
						case 11:
							if (_v3.b) {
								break _v3$1;
							} else {
								return true;
							}
						case 12:
							if (_v3.b) {
								break _v3$1;
							} else {
								return true;
							}
						case 13:
							if (_v3.b) {
								break _v3$1;
							} else {
								return true;
							}
						case 18:
							if (_v3.b) {
								break _v3$1;
							} else {
								return true;
							}
						case 19:
							if (_v3.b) {
								break _v3$1;
							} else {
								return true;
							}
						case 20:
							if (_v3.b) {
								break _v3$1;
							} else {
								var _v5 = _v3.c;
								return true;
							}
						case 21:
							if (_v3.b) {
								break _v3$1;
							} else {
								return true;
							}
						case 22:
							if (_v3.b) {
								break _v3$1;
							} else {
								var _v6 = _v3.c;
								return true;
							}
						default:
							if (_v3.b) {
								break _v3$1;
							} else {
								return false;
							}
					}
				}
			}
			return true;
		};
		var removeParens = function (expr) {
			if (expr.$ === 14) {
				var innerExpr = expr.a;
				return shouldRemove(
					$mdgriffith$elm_codegen$Internal$Compiler$denode(innerExpr)) ? removeParens(
					$mdgriffith$elm_codegen$Internal$Compiler$denode(innerExpr)) : expr;
			} else {
				return expr;
			}
		};
		var addParens = function (expr) {
			var _v1 = _Utils_Tuple3(context.O, context.N, expr);
			_v1$4:
			while (true) {
				if ((!_v1.a) && (!_v1.b)) {
					switch (_v1.c.$) {
						case 15:
							return $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression(
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(expr));
						case 16:
							return $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression(
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(expr));
						case 17:
							return $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression(
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(expr));
						case 4:
							var _v2 = _v1.c;
							return $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression(
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(expr));
						default:
							break _v1$4;
					}
				} else {
					break _v1$4;
				}
			}
			return expr;
		};
		return addParens(
			removeParens(expression));
	});
var $the_sett$elm_pretty_printer$Internals$Column = function (a) {
	return {$: 7, a: a};
};
var $the_sett$elm_pretty_printer$Pretty$column = $the_sett$elm_pretty_printer$Internals$Column;
var $the_sett$elm_pretty_printer$Internals$Nesting = function (a) {
	return {$: 6, a: a};
};
var $the_sett$elm_pretty_printer$Pretty$nesting = $the_sett$elm_pretty_printer$Internals$Nesting;
var $the_sett$elm_pretty_printer$Pretty$align = function (doc) {
	return $the_sett$elm_pretty_printer$Pretty$column(
		function (currentColumn) {
			return $the_sett$elm_pretty_printer$Pretty$nesting(
				function (indentLvl) {
					return A2($the_sett$elm_pretty_printer$Pretty$nest, currentColumn - indentLvl, doc);
				});
		});
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $mdgriffith$elm_codegen$Internal$Write$decrementIndent = F2(
	function (currentIndent, spaces) {
		var modded = A2($elm$core$Basics$modBy, 4, currentIndent - spaces);
		return (!modded) ? 4 : modded;
	});
var $mdgriffith$elm_codegen$Internal$Write$doubleLines = $the_sett$elm_pretty_printer$Pretty$join(
	A2($the_sett$elm_pretty_printer$Pretty$a, $the_sett$elm_pretty_printer$Pretty$line, $the_sett$elm_pretty_printer$Pretty$line));
var $mdgriffith$elm_codegen$Internal$Write$escapeChar = function (val) {
	switch (val) {
		case '\\':
			return '\\\\';
		case '\'':
			return '\\\'';
		case '\t':
			return '\\t';
		case '\n':
			return '\\n';
		default:
			var c = val;
			return $elm$core$String$fromChar(c);
	}
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $the_sett$elm_pretty_printer$Internals$copy = F2(
	function (i, s) {
		return (!i) ? '' : _Utils_ap(
			s,
			A2($the_sett$elm_pretty_printer$Internals$copy, i - 1, s));
	});
var $the_sett$elm_pretty_printer$Pretty$hang = F2(
	function (spaces, doc) {
		return $the_sett$elm_pretty_printer$Pretty$align(
			A2($the_sett$elm_pretty_printer$Pretty$nest, spaces, doc));
	});
var $the_sett$elm_pretty_printer$Pretty$indent = F2(
	function (spaces, doc) {
		return A2(
			$the_sett$elm_pretty_printer$Pretty$hang,
			spaces,
			A2(
				$the_sett$elm_pretty_printer$Pretty$append,
				$the_sett$elm_pretty_printer$Pretty$string(
					A2($the_sett$elm_pretty_printer$Internals$copy, spaces, ' ')),
				doc));
	});
var $mdgriffith$elm_codegen$Internal$Write$optionalGroup = F2(
	function (flag, doc) {
		return flag ? doc : $the_sett$elm_pretty_printer$Pretty$group(doc);
	});
var $mdgriffith$elm_codegen$Internal$Write$precedence = function (symbol) {
	switch (symbol) {
		case '>>':
			return 9;
		case '<<':
			return 9;
		case '^':
			return 8;
		case '*':
			return 7;
		case '/':
			return 7;
		case '//':
			return 7;
		case '%':
			return 7;
		case 'rem':
			return 7;
		case '+':
			return 6;
		case '-':
			return 6;
		case '++':
			return 5;
		case '::':
			return 5;
		case '==':
			return 4;
		case '/=':
			return 4;
		case '<':
			return 4;
		case '>':
			return 4;
		case '<=':
			return 4;
		case '>=':
			return 4;
		case '&&':
			return 3;
		case '||':
			return 2;
		case '|>':
			return 0;
		case '<|':
			return 0;
		default:
			return 0;
	}
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$ParenthesizedPattern = function (a) {
	return {$: 14, a: a};
};
var $mdgriffith$elm_codegen$Internal$Write$adjustPatternParentheses = F2(
	function (isTop, pattern) {
		var shouldRemove = function (pat) {
			var _v5 = _Utils_Tuple2(isTop, pat);
			_v5$2:
			while (true) {
				switch (_v5.b.$) {
					case 12:
						if (!_v5.a) {
							var _v6 = _v5.b;
							return false;
						} else {
							break _v5$2;
						}
					case 13:
						var _v7 = _v5.b;
						return false;
					default:
						break _v5$2;
				}
			}
			return isTop;
		};
		var removeParens = function (pat) {
			if (pat.$ === 14) {
				var innerPat = pat.a;
				return shouldRemove(
					$mdgriffith$elm_codegen$Internal$Compiler$denode(innerPat)) ? removeParens(
					$mdgriffith$elm_codegen$Internal$Compiler$denode(innerPat)) : pat;
			} else {
				return pat;
			}
		};
		var addParens = function (pat) {
			var _v1 = _Utils_Tuple2(isTop, pat);
			_v1$2:
			while (true) {
				if (!_v1.a) {
					switch (_v1.b.$) {
						case 12:
							if (_v1.b.b.b) {
								var _v2 = _v1.b;
								var _v3 = _v2.b;
								return $stil4m$elm_syntax$Elm$Syntax$Pattern$ParenthesizedPattern(
									$mdgriffith$elm_codegen$Internal$Compiler$nodify(pat));
							} else {
								break _v1$2;
							}
						case 13:
							var _v4 = _v1.b;
							return $stil4m$elm_syntax$Elm$Syntax$Pattern$ParenthesizedPattern(
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(pat));
						default:
							break _v1$2;
					}
				} else {
					break _v1$2;
				}
			}
			return pat;
		};
		return addParens(
			removeParens(pattern));
	});
var $the_sett$elm_pretty_printer$Pretty$braces = function (doc) {
	return A3(
		$the_sett$elm_pretty_printer$Pretty$surround,
		$the_sett$elm_pretty_printer$Pretty$char('{'),
		$the_sett$elm_pretty_printer$Pretty$char('}'),
		doc);
};
var $mdgriffith$elm_codegen$Internal$Write$quotes = function (doc) {
	return A3(
		$the_sett$elm_pretty_printer$Pretty$surround,
		$the_sett$elm_pretty_printer$Pretty$char('\"'),
		$the_sett$elm_pretty_printer$Pretty$char('\"'),
		doc);
};
var $mdgriffith$elm_codegen$Internal$Write$singleQuotes = function (doc) {
	return A3(
		$the_sett$elm_pretty_printer$Pretty$surround,
		$the_sett$elm_pretty_printer$Pretty$char('\''),
		$the_sett$elm_pretty_printer$Pretty$char('\''),
		doc);
};
var $elm$core$String$fromList = _String_fromList;
var $rtfeldman$elm_hex$Hex$unsafeToDigit = function (num) {
	unsafeToDigit:
	while (true) {
		switch (num) {
			case 0:
				return '0';
			case 1:
				return '1';
			case 2:
				return '2';
			case 3:
				return '3';
			case 4:
				return '4';
			case 5:
				return '5';
			case 6:
				return '6';
			case 7:
				return '7';
			case 8:
				return '8';
			case 9:
				return '9';
			case 10:
				return 'a';
			case 11:
				return 'b';
			case 12:
				return 'c';
			case 13:
				return 'd';
			case 14:
				return 'e';
			case 15:
				return 'f';
			default:
				var $temp$num = num;
				num = $temp$num;
				continue unsafeToDigit;
		}
	}
};
var $rtfeldman$elm_hex$Hex$unsafePositiveToDigits = F2(
	function (digits, num) {
		unsafePositiveToDigits:
		while (true) {
			if (num < 16) {
				return A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(num),
					digits);
			} else {
				var $temp$digits = A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(
						A2($elm$core$Basics$modBy, 16, num)),
					digits),
					$temp$num = (num / 16) | 0;
				digits = $temp$digits;
				num = $temp$num;
				continue unsafePositiveToDigits;
			}
		}
	});
var $rtfeldman$elm_hex$Hex$toString = function (num) {
	return $elm$core$String$fromList(
		(num < 0) ? A2(
			$elm$core$List$cons,
			'-',
			A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, -num)) : A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, num));
};
var $mdgriffith$elm_codegen$Internal$Write$prettyPatternInner = F3(
	function (aliases, isTop, pattern) {
		var _v0 = A2($mdgriffith$elm_codegen$Internal$Write$adjustPatternParentheses, isTop, pattern);
		switch (_v0.$) {
			case 0:
				return $the_sett$elm_pretty_printer$Pretty$string('_');
			case 1:
				return $the_sett$elm_pretty_printer$Pretty$string('()');
			case 2:
				var val = _v0.a;
				return $mdgriffith$elm_codegen$Internal$Write$singleQuotes(
					$the_sett$elm_pretty_printer$Pretty$string(
						$mdgriffith$elm_codegen$Internal$Write$escapeChar(val)));
			case 3:
				var val = _v0.a;
				return $mdgriffith$elm_codegen$Internal$Write$quotes(
					$the_sett$elm_pretty_printer$Pretty$string(val));
			case 4:
				var val = _v0.a;
				return $the_sett$elm_pretty_printer$Pretty$string(
					$elm$core$String$fromInt(val));
			case 5:
				var val = _v0.a;
				return $the_sett$elm_pretty_printer$Pretty$string(
					$rtfeldman$elm_hex$Hex$toString(val));
			case 6:
				var val = _v0.a;
				return $the_sett$elm_pretty_printer$Pretty$string(
					$elm$core$String$fromFloat(val));
			case 7:
				var vals = _v0.a;
				return $the_sett$elm_pretty_printer$Pretty$parens(
					A2(
						$the_sett$elm_pretty_printer$Pretty$a,
						$the_sett$elm_pretty_printer$Pretty$space,
						A2(
							$the_sett$elm_pretty_printer$Pretty$a,
							A2(
								$the_sett$elm_pretty_printer$Pretty$join,
								$the_sett$elm_pretty_printer$Pretty$string(', '),
								A2(
									$elm$core$List$map,
									A2($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, true),
									$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(vals))),
							$the_sett$elm_pretty_printer$Pretty$space)));
			case 8:
				var fields = _v0.a;
				return $the_sett$elm_pretty_printer$Pretty$braces(
					A3(
						$the_sett$elm_pretty_printer$Pretty$surround,
						$the_sett$elm_pretty_printer$Pretty$space,
						$the_sett$elm_pretty_printer$Pretty$space,
						A2(
							$the_sett$elm_pretty_printer$Pretty$join,
							$the_sett$elm_pretty_printer$Pretty$string(', '),
							A2(
								$elm$core$List$map,
								$the_sett$elm_pretty_printer$Pretty$string,
								$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(fields)))));
			case 9:
				var hdPat = _v0.a;
				var tlPat = _v0.b;
				return $the_sett$elm_pretty_printer$Pretty$words(
					_List_fromArray(
						[
							A3(
							$mdgriffith$elm_codegen$Internal$Write$prettyPatternInner,
							aliases,
							false,
							$mdgriffith$elm_codegen$Internal$Compiler$denode(hdPat)),
							$the_sett$elm_pretty_printer$Pretty$string('::'),
							A3(
							$mdgriffith$elm_codegen$Internal$Write$prettyPatternInner,
							aliases,
							false,
							$mdgriffith$elm_codegen$Internal$Compiler$denode(tlPat))
						]));
			case 10:
				var listPats = _v0.a;
				if (!listPats.b) {
					return $the_sett$elm_pretty_printer$Pretty$string('[]');
				} else {
					var open = A2(
						$the_sett$elm_pretty_printer$Pretty$a,
						$the_sett$elm_pretty_printer$Pretty$space,
						$the_sett$elm_pretty_printer$Pretty$string('['));
					var close = A2(
						$the_sett$elm_pretty_printer$Pretty$a,
						$the_sett$elm_pretty_printer$Pretty$string(']'),
						$the_sett$elm_pretty_printer$Pretty$space);
					return A3(
						$the_sett$elm_pretty_printer$Pretty$surround,
						open,
						close,
						A2(
							$the_sett$elm_pretty_printer$Pretty$join,
							$the_sett$elm_pretty_printer$Pretty$string(', '),
							A2(
								$elm$core$List$map,
								A2($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false),
								$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(listPats))));
				}
			case 11:
				var _var = _v0.a;
				return $the_sett$elm_pretty_printer$Pretty$string(_var);
			case 12:
				var qnRef = _v0.a;
				var listPats = _v0.b;
				return $the_sett$elm_pretty_printer$Pretty$words(
					A2(
						$elm$core$List$cons,
						A2(
							$the_sett$elm_pretty_printer$Pretty$a,
							$the_sett$elm_pretty_printer$Pretty$string(qnRef.ao),
							A2($mdgriffith$elm_codegen$Internal$Write$prettyModuleNameDot, aliases, qnRef.a1)),
						A2(
							$elm$core$List$map,
							A2($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false),
							$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(listPats))));
			case 13:
				var pat = _v0.a;
				var name = _v0.b;
				return $the_sett$elm_pretty_printer$Pretty$words(
					_List_fromArray(
						[
							A3(
							$mdgriffith$elm_codegen$Internal$Write$prettyPatternInner,
							aliases,
							false,
							$mdgriffith$elm_codegen$Internal$Compiler$denode(pat)),
							$the_sett$elm_pretty_printer$Pretty$string('as'),
							$the_sett$elm_pretty_printer$Pretty$string(
							$mdgriffith$elm_codegen$Internal$Compiler$denode(name))
						]));
			default:
				var pat = _v0.a;
				return $the_sett$elm_pretty_printer$Pretty$parens(
					A3(
						$mdgriffith$elm_codegen$Internal$Write$prettyPatternInner,
						aliases,
						true,
						$mdgriffith$elm_codegen$Internal$Compiler$denode(pat)));
		}
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyArgs = F2(
	function (aliases, args) {
		return $the_sett$elm_pretty_printer$Pretty$words(
			A2(
				$elm$core$List$map,
				A2($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false),
				args));
	});
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $mdgriffith$elm_codegen$Internal$Write$escape = function (val) {
	return A3(
		$elm$core$String$replace,
		'\t',
		'\\t',
		A3(
			$elm$core$String$replace,
			'\n',
			'\\n',
			A3(
				$elm$core$String$replace,
				'\"',
				'\\\"',
				A3($elm$core$String$replace, '\\', '\\\\', val))));
};
var $mdgriffith$elm_codegen$Internal$Write$tripleQuotes = function (doc) {
	return A3(
		$the_sett$elm_pretty_printer$Pretty$surround,
		$the_sett$elm_pretty_printer$Pretty$string('\"\"\"'),
		$the_sett$elm_pretty_printer$Pretty$string('\"\"\"'),
		doc);
};
var $mdgriffith$elm_codegen$Internal$Write$prettyLiteral = function (val) {
	return A2($elm$core$String$contains, '\n', val) ? $mdgriffith$elm_codegen$Internal$Write$tripleQuotes(
		$the_sett$elm_pretty_printer$Pretty$string(val)) : $mdgriffith$elm_codegen$Internal$Write$quotes(
		$the_sett$elm_pretty_printer$Pretty$string(
			$mdgriffith$elm_codegen$Internal$Write$escape(val)));
};
var $mdgriffith$elm_codegen$Internal$Write$prettyPattern = F2(
	function (aliases, pattern) {
		return A3($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, true, pattern);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettySignature = F2(
	function (aliases, sig) {
		return $the_sett$elm_pretty_printer$Pretty$group(
			A2(
				$the_sett$elm_pretty_printer$Pretty$nest,
				4,
				$the_sett$elm_pretty_printer$Pretty$lines(
					_List_fromArray(
						[
							$the_sett$elm_pretty_printer$Pretty$words(
							_List_fromArray(
								[
									$the_sett$elm_pretty_printer$Pretty$string(
									$mdgriffith$elm_codegen$Internal$Compiler$denode(sig.ao)),
									$the_sett$elm_pretty_printer$Pretty$string(':')
								])),
							A2(
							$mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation,
							aliases,
							$mdgriffith$elm_codegen$Internal$Compiler$denode(sig.aN))
						]))));
	});
var $the_sett$elm_pretty_printer$Pretty$tightline = A2($the_sett$elm_pretty_printer$Internals$Line, '', '');
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $elm$core$String$toUpper = _String_toUpper;
var $mdgriffith$elm_codegen$Internal$Write$toHexString = function (val) {
	var padWithZeros = function (str) {
		var length = $elm$core$String$length(str);
		return (length < 2) ? A3($elm$core$String$padLeft, 2, '0', str) : (((length > 2) && (length < 4)) ? A3($elm$core$String$padLeft, 4, '0', str) : (((length > 4) && (length < 8)) ? A3($elm$core$String$padLeft, 8, '0', str) : str));
	};
	return '0x' + padWithZeros(
		$elm$core$String$toUpper(
			$rtfeldman$elm_hex$Hex$toString(val)));
};
var $mdgriffith$elm_codegen$Internal$Write$topContext = {N: false, O: true, kl: 11};
var $elm$core$List$unzip = function (pairs) {
	var step = F2(
		function (_v0, _v1) {
			var x = _v0.a;
			var y = _v0.b;
			var xs = _v1.a;
			var ys = _v1.b;
			return _Utils_Tuple2(
				A2($elm$core$List$cons, x, xs),
				A2($elm$core$List$cons, y, ys));
		});
	return A3(
		$elm$core$List$foldr,
		step,
		_Utils_Tuple2(_List_Nil, _List_Nil),
		pairs);
};
var $mdgriffith$elm_codegen$Internal$Write$prettyApplication = F3(
	function (aliases, indent, exprs) {
		var _v30 = A2(
			$elm$core$Tuple$mapSecond,
			$elm$core$List$any($elm$core$Basics$identity),
			$elm$core$List$unzip(
				A2(
					$elm$core$List$map,
					A3(
						$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
						aliases,
						{N: false, O: false, kl: 11},
						4),
					$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(exprs))));
		var prettyExpressions = _v30.a;
		var alwaysBreak = _v30.b;
		return _Utils_Tuple2(
			A2(
				$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
				alwaysBreak,
				$the_sett$elm_pretty_printer$Pretty$align(
					A2(
						$the_sett$elm_pretty_printer$Pretty$nest,
						indent,
						$the_sett$elm_pretty_printer$Pretty$lines(prettyExpressions)))),
			alwaysBreak);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyCaseBlock = F3(
	function (aliases, indent, caseBlock) {
		var prettyCase = function (_v29) {
			var pattern = _v29.a;
			var expr = _v29.b;
			return A2(
				$the_sett$elm_pretty_printer$Pretty$indent,
				indent,
				A2(
					$the_sett$elm_pretty_printer$Pretty$a,
					A2(
						$the_sett$elm_pretty_printer$Pretty$indent,
						4,
						A4(
							$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
							aliases,
							$mdgriffith$elm_codegen$Internal$Write$topContext,
							4,
							$mdgriffith$elm_codegen$Internal$Compiler$denode(expr)).a),
					A2(
						$the_sett$elm_pretty_printer$Pretty$a,
						$the_sett$elm_pretty_printer$Pretty$line,
						A2(
							$the_sett$elm_pretty_printer$Pretty$a,
							$the_sett$elm_pretty_printer$Pretty$string(' ->'),
							A2(
								$mdgriffith$elm_codegen$Internal$Write$prettyPattern,
								aliases,
								$mdgriffith$elm_codegen$Internal$Compiler$denode(pattern))))));
		};
		var patternsPart = $mdgriffith$elm_codegen$Internal$Write$doubleLines(
			A2($elm$core$List$map, prettyCase, caseBlock.j));
		var casePart = function () {
			var _v28 = A4(
				$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
				aliases,
				$mdgriffith$elm_codegen$Internal$Write$topContext,
				4,
				$mdgriffith$elm_codegen$Internal$Compiler$denode(caseBlock.jr));
			var caseExpression = _v28.a;
			var alwaysBreak = _v28.b;
			return A2(
				$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
				alwaysBreak,
				$the_sett$elm_pretty_printer$Pretty$lines(
					_List_fromArray(
						[
							A2(
							$the_sett$elm_pretty_printer$Pretty$nest,
							indent,
							A2(
								$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
								alwaysBreak,
								$the_sett$elm_pretty_printer$Pretty$lines(
									_List_fromArray(
										[
											$the_sett$elm_pretty_printer$Pretty$string('case'),
											caseExpression
										])))),
							$the_sett$elm_pretty_printer$Pretty$string('of')
						])));
		}();
		return _Utils_Tuple2(
			$the_sett$elm_pretty_printer$Pretty$align(
				$the_sett$elm_pretty_printer$Pretty$lines(
					_List_fromArray(
						[casePart, patternsPart]))),
			true);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyExpression = F2(
	function (aliases, expression) {
		return A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, $mdgriffith$elm_codegen$Internal$Write$topContext, 4, expression).a;
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner = F4(
	function (aliases, context, indent, expression) {
		var _v26 = A2($mdgriffith$elm_codegen$Internal$Write$adjustExpressionParentheses, context, expression);
		switch (_v26.$) {
			case 0:
				return _Utils_Tuple2(
					$the_sett$elm_pretty_printer$Pretty$string('()'),
					false);
			case 1:
				var exprs = _v26.a;
				return A3($mdgriffith$elm_codegen$Internal$Write$prettyApplication, aliases, indent, exprs);
			case 2:
				var symbol = _v26.a;
				var dir = _v26.b;
				var exprl = _v26.c;
				var exprr = _v26.d;
				return A6($mdgriffith$elm_codegen$Internal$Write$prettyOperatorApplication, aliases, indent, symbol, dir, exprl, exprr);
			case 3:
				var modl = _v26.a;
				var val = _v26.b;
				return _Utils_Tuple2(
					A2(
						$the_sett$elm_pretty_printer$Pretty$a,
						$the_sett$elm_pretty_printer$Pretty$string(val),
						A2($mdgriffith$elm_codegen$Internal$Write$prettyModuleNameDot, aliases, modl)),
					false);
			case 4:
				var exprBool = _v26.a;
				var exprTrue = _v26.b;
				var exprFalse = _v26.c;
				return A5($mdgriffith$elm_codegen$Internal$Write$prettyIfBlock, aliases, indent, exprBool, exprTrue, exprFalse);
			case 5:
				var symbol = _v26.a;
				return _Utils_Tuple2(
					$the_sett$elm_pretty_printer$Pretty$parens(
						$the_sett$elm_pretty_printer$Pretty$string(symbol)),
					false);
			case 6:
				var symbol = _v26.a;
				return _Utils_Tuple2(
					$the_sett$elm_pretty_printer$Pretty$string(symbol),
					false);
			case 7:
				var val = _v26.a;
				return _Utils_Tuple2(
					$the_sett$elm_pretty_printer$Pretty$string(
						$elm$core$String$fromInt(val)),
					false);
			case 8:
				var val = _v26.a;
				return _Utils_Tuple2(
					$the_sett$elm_pretty_printer$Pretty$string(
						$mdgriffith$elm_codegen$Internal$Write$toHexString(val)),
					false);
			case 9:
				var val = _v26.a;
				return _Utils_Tuple2(
					$the_sett$elm_pretty_printer$Pretty$string(
						$elm$core$String$fromFloat(val)),
					false);
			case 10:
				var expr = _v26.a;
				var _v27 = A4(
					$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
					aliases,
					$mdgriffith$elm_codegen$Internal$Write$topContext,
					4,
					$mdgriffith$elm_codegen$Internal$Compiler$denode(expr));
				var prettyExpr = _v27.a;
				var alwaysBreak = _v27.b;
				return _Utils_Tuple2(
					A2(
						$the_sett$elm_pretty_printer$Pretty$a,
						prettyExpr,
						$the_sett$elm_pretty_printer$Pretty$string('-')),
					alwaysBreak);
			case 11:
				var val = _v26.a;
				return _Utils_Tuple2(
					$mdgriffith$elm_codegen$Internal$Write$prettyLiteral(val),
					false);
			case 12:
				var val = _v26.a;
				return _Utils_Tuple2(
					$mdgriffith$elm_codegen$Internal$Write$singleQuotes(
						$the_sett$elm_pretty_printer$Pretty$string(
							$mdgriffith$elm_codegen$Internal$Write$escapeChar(val))),
					false);
			case 13:
				var exprs = _v26.a;
				return A3($mdgriffith$elm_codegen$Internal$Write$prettyTupledExpression, aliases, indent, exprs);
			case 14:
				var expr = _v26.a;
				return A3($mdgriffith$elm_codegen$Internal$Write$prettyParenthesizedExpression, aliases, indent, expr);
			case 15:
				var letBlock = _v26.a;
				return A3($mdgriffith$elm_codegen$Internal$Write$prettyLetBlock, aliases, indent, letBlock);
			case 16:
				var caseBlock = _v26.a;
				return A3($mdgriffith$elm_codegen$Internal$Write$prettyCaseBlock, aliases, indent, caseBlock);
			case 17:
				var lambda = _v26.a;
				return A3($mdgriffith$elm_codegen$Internal$Write$prettyLambdaExpression, aliases, indent, lambda);
			case 18:
				var setters = _v26.a;
				return A2($mdgriffith$elm_codegen$Internal$Write$prettyRecordExpr, aliases, setters);
			case 19:
				var exprs = _v26.a;
				return A3($mdgriffith$elm_codegen$Internal$Write$prettyList, aliases, indent, exprs);
			case 20:
				var expr = _v26.a;
				var field = _v26.b;
				return A3($mdgriffith$elm_codegen$Internal$Write$prettyRecordAccess, aliases, expr, field);
			case 21:
				var field = _v26.a;
				return _Utils_Tuple2(
					$the_sett$elm_pretty_printer$Pretty$string(field),
					false);
			case 22:
				var _var = _v26.a;
				var setters = _v26.b;
				return A4($mdgriffith$elm_codegen$Internal$Write$prettyRecordUpdateExpression, aliases, indent, _var, setters);
			default:
				var val = _v26.a;
				return _Utils_Tuple2(
					$the_sett$elm_pretty_printer$Pretty$string('glsl'),
					true);
		}
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyFun = F2(
	function (aliases, fn) {
		return $the_sett$elm_pretty_printer$Pretty$lines(
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_codegen$Internal$Write$prettyMaybe,
					$mdgriffith$elm_codegen$Internal$Write$prettyDocumentation,
					$mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(fn.aU)),
					A2(
					$mdgriffith$elm_codegen$Internal$Write$prettyMaybe,
					$mdgriffith$elm_codegen$Internal$Write$prettySignature(aliases),
					$mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(fn.kE)),
					A2(
					$mdgriffith$elm_codegen$Internal$Write$prettyFunctionImplementation,
					aliases,
					$mdgriffith$elm_codegen$Internal$Compiler$denode(fn.jf))
				]));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyFunctionImplementation = F2(
	function (aliases, impl) {
		return A2(
			$the_sett$elm_pretty_printer$Pretty$nest,
			4,
			A2(
				$the_sett$elm_pretty_printer$Pretty$a,
				A2(
					$mdgriffith$elm_codegen$Internal$Write$prettyExpression,
					aliases,
					$mdgriffith$elm_codegen$Internal$Compiler$denode(impl.jr)),
				A2(
					$the_sett$elm_pretty_printer$Pretty$a,
					$the_sett$elm_pretty_printer$Pretty$line,
					$the_sett$elm_pretty_printer$Pretty$words(
						_List_fromArray(
							[
								$the_sett$elm_pretty_printer$Pretty$string(
								$mdgriffith$elm_codegen$Internal$Compiler$denode(impl.ao)),
								A2(
								$mdgriffith$elm_codegen$Internal$Write$prettyArgs,
								aliases,
								$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(impl.aQ)),
								$the_sett$elm_pretty_printer$Pretty$string('=')
							])))));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyIfBlock = F5(
	function (aliases, indent, exprBool, exprTrue, exprFalse) {
		var innerIfBlock = F3(
			function (innerExprBool, innerExprTrue, innerExprFalse) {
				var truePart = A2(
					$the_sett$elm_pretty_printer$Pretty$indent,
					indent,
					A4(
						$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
						aliases,
						$mdgriffith$elm_codegen$Internal$Write$topContext,
						4,
						$mdgriffith$elm_codegen$Internal$Compiler$denode(innerExprTrue)).a);
				var ifPart = function () {
					var _v25 = A4(
						$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
						aliases,
						$mdgriffith$elm_codegen$Internal$Write$topContext,
						4,
						$mdgriffith$elm_codegen$Internal$Compiler$denode(innerExprBool));
					var prettyBoolExpr = _v25.a;
					var alwaysBreak = _v25.b;
					return A2(
						$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
						alwaysBreak,
						$the_sett$elm_pretty_printer$Pretty$lines(
							_List_fromArray(
								[
									A2(
									$the_sett$elm_pretty_printer$Pretty$nest,
									indent,
									A2(
										$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
										alwaysBreak,
										$the_sett$elm_pretty_printer$Pretty$lines(
											_List_fromArray(
												[
													$the_sett$elm_pretty_printer$Pretty$string('if'),
													A4(
													$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
													aliases,
													$mdgriffith$elm_codegen$Internal$Write$topContext,
													4,
													$mdgriffith$elm_codegen$Internal$Compiler$denode(innerExprBool)).a
												])))),
									$the_sett$elm_pretty_printer$Pretty$string('then')
								])));
				}();
				var falsePart = function () {
					var _v24 = $mdgriffith$elm_codegen$Internal$Compiler$denode(innerExprFalse);
					if (_v24.$ === 4) {
						var nestedExprBool = _v24.a;
						var nestedExprTrue = _v24.b;
						var nestedExprFalse = _v24.c;
						return A3(innerIfBlock, nestedExprBool, nestedExprTrue, nestedExprFalse);
					} else {
						return _List_fromArray(
							[
								A2(
								$the_sett$elm_pretty_printer$Pretty$indent,
								indent,
								A4(
									$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
									aliases,
									$mdgriffith$elm_codegen$Internal$Write$topContext,
									4,
									$mdgriffith$elm_codegen$Internal$Compiler$denode(innerExprFalse)).a)
							]);
					}
				}();
				var elsePart = A2(
					$the_sett$elm_pretty_printer$Pretty$a,
					$the_sett$elm_pretty_printer$Pretty$string('else'),
					$the_sett$elm_pretty_printer$Pretty$line);
				var context = $mdgriffith$elm_codegen$Internal$Write$topContext;
				if (!falsePart.b) {
					return _List_Nil;
				} else {
					if (!falsePart.b.b) {
						var falseExpr = falsePart.a;
						return _List_fromArray(
							[ifPart, truePart, elsePart, falseExpr]);
					} else {
						var hd = falsePart.a;
						var tl = falsePart.b;
						return A2(
							$elm$core$List$append,
							_List_fromArray(
								[
									ifPart,
									truePart,
									$the_sett$elm_pretty_printer$Pretty$words(
									_List_fromArray(
										[elsePart, hd]))
								]),
							tl);
					}
				}
			});
		var prettyExpressions = A3(innerIfBlock, exprBool, exprTrue, exprFalse);
		return _Utils_Tuple2(
			$the_sett$elm_pretty_printer$Pretty$align(
				$the_sett$elm_pretty_printer$Pretty$lines(prettyExpressions)),
			true);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyLambdaExpression = F3(
	function (aliases, indent, lambda) {
		var _v22 = A4(
			$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
			aliases,
			$mdgriffith$elm_codegen$Internal$Write$topContext,
			4,
			$mdgriffith$elm_codegen$Internal$Compiler$denode(lambda.jr));
		var prettyExpr = _v22.a;
		var alwaysBreak = _v22.b;
		return _Utils_Tuple2(
			A2(
				$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
				alwaysBreak,
				$the_sett$elm_pretty_printer$Pretty$align(
					A2(
						$the_sett$elm_pretty_printer$Pretty$nest,
						indent,
						$the_sett$elm_pretty_printer$Pretty$lines(
							_List_fromArray(
								[
									A2(
									$the_sett$elm_pretty_printer$Pretty$a,
									$the_sett$elm_pretty_printer$Pretty$string(' ->'),
									A2(
										$the_sett$elm_pretty_printer$Pretty$a,
										$the_sett$elm_pretty_printer$Pretty$words(
											A2(
												$elm$core$List$map,
												A2($mdgriffith$elm_codegen$Internal$Write$prettyPatternInner, aliases, false),
												$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(lambda.B))),
										$the_sett$elm_pretty_printer$Pretty$string('\\'))),
									prettyExpr
								]))))),
			alwaysBreak);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyLetBlock = F3(
	function (aliases, indent, letBlock) {
		return _Utils_Tuple2(
			$the_sett$elm_pretty_printer$Pretty$align(
				$the_sett$elm_pretty_printer$Pretty$lines(
					_List_fromArray(
						[
							$the_sett$elm_pretty_printer$Pretty$string('let'),
							A2(
							$the_sett$elm_pretty_printer$Pretty$indent,
							indent,
							$mdgriffith$elm_codegen$Internal$Write$doubleLines(
								A2(
									$elm$core$List$map,
									A2($mdgriffith$elm_codegen$Internal$Write$prettyLetDeclaration, aliases, indent),
									$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(letBlock.ai)))),
							$the_sett$elm_pretty_printer$Pretty$string('in'),
							A4(
							$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
							aliases,
							$mdgriffith$elm_codegen$Internal$Write$topContext,
							4,
							$mdgriffith$elm_codegen$Internal$Compiler$denode(letBlock.jr)).a
						]))),
			true);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyLetDeclaration = F3(
	function (aliases, indent, letDecl) {
		if (!letDecl.$) {
			var fn = letDecl.a;
			return A2($mdgriffith$elm_codegen$Internal$Write$prettyFun, aliases, fn);
		} else {
			var pattern = letDecl.a;
			var expr = letDecl.b;
			return A2(
				$the_sett$elm_pretty_printer$Pretty$a,
				A2(
					$the_sett$elm_pretty_printer$Pretty$indent,
					indent,
					A4(
						$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
						aliases,
						$mdgriffith$elm_codegen$Internal$Write$topContext,
						4,
						$mdgriffith$elm_codegen$Internal$Compiler$denode(expr)).a),
				A2(
					$the_sett$elm_pretty_printer$Pretty$a,
					$the_sett$elm_pretty_printer$Pretty$line,
					$the_sett$elm_pretty_printer$Pretty$words(
						_List_fromArray(
							[
								A3(
								$mdgriffith$elm_codegen$Internal$Write$prettyPatternInner,
								aliases,
								false,
								$mdgriffith$elm_codegen$Internal$Compiler$denode(pattern)),
								$the_sett$elm_pretty_printer$Pretty$string('=')
							]))));
		}
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyList = F3(
	function (aliases, indent, exprs) {
		var open = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$space,
			$the_sett$elm_pretty_printer$Pretty$string('['));
		var close = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$string(']'),
			$the_sett$elm_pretty_printer$Pretty$line);
		if (!exprs.b) {
			return _Utils_Tuple2(
				$the_sett$elm_pretty_printer$Pretty$string('[]'),
				false);
		} else {
			var _v20 = A2(
				$elm$core$Tuple$mapSecond,
				$elm$core$List$any($elm$core$Basics$identity),
				$elm$core$List$unzip(
					A2(
						$elm$core$List$map,
						A3(
							$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
							aliases,
							$mdgriffith$elm_codegen$Internal$Write$topContext,
							A2($mdgriffith$elm_codegen$Internal$Write$decrementIndent, indent, 2)),
						$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(exprs))));
			var prettyExpressions = _v20.a;
			var alwaysBreak = _v20.b;
			return _Utils_Tuple2(
				A2(
					$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
					alwaysBreak,
					$the_sett$elm_pretty_printer$Pretty$align(
						A3(
							$the_sett$elm_pretty_printer$Pretty$surround,
							open,
							close,
							A2($the_sett$elm_pretty_printer$Pretty$separators, ', ', prettyExpressions)))),
				alwaysBreak);
		}
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyOperatorApplication = F6(
	function (aliases, indent, symbol, dir, exprl, exprr) {
		return (symbol === '<|') ? A6($mdgriffith$elm_codegen$Internal$Write$prettyOperatorApplicationLeft, aliases, indent, symbol, dir, exprl, exprr) : A6($mdgriffith$elm_codegen$Internal$Write$prettyOperatorApplicationRight, aliases, indent, symbol, dir, exprl, exprr);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyOperatorApplicationLeft = F6(
	function (aliases, indent, symbol, _v16, exprl, exprr) {
		var context = {
			N: true,
			O: false,
			kl: $mdgriffith$elm_codegen$Internal$Write$precedence(symbol)
		};
		var _v17 = A4(
			$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
			aliases,
			context,
			4,
			$mdgriffith$elm_codegen$Internal$Compiler$denode(exprr));
		var prettyExpressionRight = _v17.a;
		var alwaysBreakRight = _v17.b;
		var _v18 = A4(
			$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
			aliases,
			context,
			4,
			$mdgriffith$elm_codegen$Internal$Compiler$denode(exprl));
		var prettyExpressionLeft = _v18.a;
		var alwaysBreakLeft = _v18.b;
		var alwaysBreak = alwaysBreakLeft || alwaysBreakRight;
		return _Utils_Tuple2(
			A2(
				$the_sett$elm_pretty_printer$Pretty$nest,
				4,
				A2(
					$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
					alwaysBreak,
					$the_sett$elm_pretty_printer$Pretty$lines(
						_List_fromArray(
							[
								$the_sett$elm_pretty_printer$Pretty$words(
								_List_fromArray(
									[
										prettyExpressionLeft,
										$the_sett$elm_pretty_printer$Pretty$string(symbol)
									])),
								prettyExpressionRight
							])))),
			alwaysBreak);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyOperatorApplicationRight = F6(
	function (aliases, indent, symbol, _v11, exprl, exprr) {
		var expandExpr = F3(
			function (innerIndent, context, expr) {
				if (expr.$ === 2) {
					var sym = expr.a;
					var left = expr.c;
					var right = expr.d;
					return A4(innerOpApply, false, sym, left, right);
				} else {
					return _List_fromArray(
						[
							A4($mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner, aliases, context, innerIndent, expr)
						]);
				}
			});
		var innerOpApply = F4(
			function (isTop, sym, left, right) {
				var innerIndent = A2(
					$mdgriffith$elm_codegen$Internal$Write$decrementIndent,
					4,
					$elm$core$String$length(symbol) + 1);
				var leftIndent = isTop ? indent : innerIndent;
				var context = {
					N: '<|' === sym,
					O: false,
					kl: $mdgriffith$elm_codegen$Internal$Write$precedence(sym)
				};
				var rightSide = A3(
					expandExpr,
					innerIndent,
					context,
					$mdgriffith$elm_codegen$Internal$Compiler$denode(right));
				if (rightSide.b) {
					var _v14 = rightSide.a;
					var hdExpr = _v14.a;
					var hdBreak = _v14.b;
					var tl = rightSide.b;
					return A2(
						$elm$core$List$append,
						A3(
							expandExpr,
							leftIndent,
							context,
							$mdgriffith$elm_codegen$Internal$Compiler$denode(left)),
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								A2(
									$the_sett$elm_pretty_printer$Pretty$a,
									hdExpr,
									A2(
										$the_sett$elm_pretty_printer$Pretty$a,
										$the_sett$elm_pretty_printer$Pretty$space,
										$the_sett$elm_pretty_printer$Pretty$string(sym))),
								hdBreak),
							tl));
				} else {
					return _List_Nil;
				}
			});
		var _v12 = A2(
			$elm$core$Tuple$mapSecond,
			$elm$core$List$any($elm$core$Basics$identity),
			$elm$core$List$unzip(
				A4(innerOpApply, true, symbol, exprl, exprr)));
		var prettyExpressions = _v12.a;
		var alwaysBreak = _v12.b;
		return _Utils_Tuple2(
			A2(
				$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
				alwaysBreak,
				$the_sett$elm_pretty_printer$Pretty$align(
					A2(
						$the_sett$elm_pretty_printer$Pretty$join,
						A2($the_sett$elm_pretty_printer$Pretty$nest, indent, $the_sett$elm_pretty_printer$Pretty$line),
						prettyExpressions))),
			alwaysBreak);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyParenthesizedExpression = F3(
	function (aliases, indent, expr) {
		var open = $the_sett$elm_pretty_printer$Pretty$string('(');
		var close = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$string(')'),
			$the_sett$elm_pretty_printer$Pretty$tightline);
		var _v10 = A4(
			$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
			aliases,
			$mdgriffith$elm_codegen$Internal$Write$topContext,
			A2($mdgriffith$elm_codegen$Internal$Write$decrementIndent, indent, 1),
			$mdgriffith$elm_codegen$Internal$Compiler$denode(expr));
		var prettyExpr = _v10.a;
		var alwaysBreak = _v10.b;
		return _Utils_Tuple2(
			A2(
				$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
				alwaysBreak,
				$the_sett$elm_pretty_printer$Pretty$align(
					A3(
						$the_sett$elm_pretty_printer$Pretty$surround,
						open,
						close,
						A2($the_sett$elm_pretty_printer$Pretty$nest, 1, prettyExpr)))),
			alwaysBreak);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyRecordAccess = F3(
	function (aliases, expr, field) {
		var _v9 = A4(
			$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
			aliases,
			$mdgriffith$elm_codegen$Internal$Write$topContext,
			4,
			$mdgriffith$elm_codegen$Internal$Compiler$denode(expr));
		var prettyExpr = _v9.a;
		var alwaysBreak = _v9.b;
		return _Utils_Tuple2(
			A2(
				$the_sett$elm_pretty_printer$Pretty$a,
				$the_sett$elm_pretty_printer$Pretty$string(
					$mdgriffith$elm_codegen$Internal$Compiler$denode(field)),
				A2($the_sett$elm_pretty_printer$Pretty$a, $mdgriffith$elm_codegen$Internal$Write$dot, prettyExpr)),
			alwaysBreak);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyRecordExpr = F2(
	function (aliases, setters) {
		var open = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$space,
			$the_sett$elm_pretty_printer$Pretty$string('{'));
		var close = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$string('}'),
			$the_sett$elm_pretty_printer$Pretty$line);
		if (!setters.b) {
			return _Utils_Tuple2(
				$the_sett$elm_pretty_printer$Pretty$string('{}'),
				false);
		} else {
			var _v8 = A2(
				$elm$core$Tuple$mapSecond,
				$elm$core$List$any($elm$core$Basics$identity),
				$elm$core$List$unzip(
					A2(
						$elm$core$List$map,
						$mdgriffith$elm_codegen$Internal$Write$prettySetter(aliases),
						$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(setters))));
			var prettyExpressions = _v8.a;
			var alwaysBreak = _v8.b;
			return _Utils_Tuple2(
				A2(
					$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
					alwaysBreak,
					$the_sett$elm_pretty_printer$Pretty$align(
						A3(
							$the_sett$elm_pretty_printer$Pretty$surround,
							open,
							close,
							A2($the_sett$elm_pretty_printer$Pretty$separators, ', ', prettyExpressions)))),
				alwaysBreak);
		}
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyRecordUpdateExpression = F4(
	function (aliases, indent, _var, setters) {
		var open = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$line,
			$the_sett$elm_pretty_printer$Pretty$words(
				_List_fromArray(
					[
						$the_sett$elm_pretty_printer$Pretty$string('{'),
						$the_sett$elm_pretty_printer$Pretty$string(
						$mdgriffith$elm_codegen$Internal$Compiler$denode(_var))
					])));
		var close = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$string('}'),
			$the_sett$elm_pretty_printer$Pretty$line);
		var addBarToFirst = function (exprs) {
			if (!exprs.b) {
				return _List_Nil;
			} else {
				var hd = exprs.a;
				var tl = exprs.b;
				return A2(
					$elm$core$List$cons,
					A2(
						$the_sett$elm_pretty_printer$Pretty$a,
						hd,
						$the_sett$elm_pretty_printer$Pretty$string('| ')),
					tl);
			}
		};
		if (!setters.b) {
			return _Utils_Tuple2(
				$the_sett$elm_pretty_printer$Pretty$string('{}'),
				false);
		} else {
			var _v5 = A2(
				$elm$core$Tuple$mapSecond,
				$elm$core$List$any($elm$core$Basics$identity),
				$elm$core$List$unzip(
					A2(
						$elm$core$List$map,
						$mdgriffith$elm_codegen$Internal$Write$prettySetter(aliases),
						$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(setters))));
			var prettyExpressions = _v5.a;
			var alwaysBreak = _v5.b;
			return _Utils_Tuple2(
				A2(
					$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
					alwaysBreak,
					$the_sett$elm_pretty_printer$Pretty$align(
						A3(
							$the_sett$elm_pretty_printer$Pretty$surround,
							$the_sett$elm_pretty_printer$Pretty$empty,
							close,
							A2(
								$the_sett$elm_pretty_printer$Pretty$nest,
								indent,
								A2(
									$the_sett$elm_pretty_printer$Pretty$a,
									A2(
										$the_sett$elm_pretty_printer$Pretty$separators,
										', ',
										addBarToFirst(prettyExpressions)),
									open))))),
				alwaysBreak);
		}
	});
var $mdgriffith$elm_codegen$Internal$Write$prettySetter = F2(
	function (aliases, _v2) {
		var fld = _v2.a;
		var val = _v2.b;
		var _v3 = A4(
			$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
			aliases,
			$mdgriffith$elm_codegen$Internal$Write$topContext,
			4,
			$mdgriffith$elm_codegen$Internal$Compiler$denode(val));
		var prettyExpr = _v3.a;
		var alwaysBreak = _v3.b;
		return _Utils_Tuple2(
			A2(
				$the_sett$elm_pretty_printer$Pretty$nest,
				4,
				A2(
					$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
					alwaysBreak,
					$the_sett$elm_pretty_printer$Pretty$lines(
						_List_fromArray(
							[
								$the_sett$elm_pretty_printer$Pretty$words(
								_List_fromArray(
									[
										$the_sett$elm_pretty_printer$Pretty$string(
										$mdgriffith$elm_codegen$Internal$Compiler$denode(fld)),
										$the_sett$elm_pretty_printer$Pretty$string('=')
									])),
								prettyExpr
							])))),
			alwaysBreak);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyTupledExpression = F3(
	function (aliases, indent, exprs) {
		var open = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$space,
			$the_sett$elm_pretty_printer$Pretty$string('('));
		var close = A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$string(')'),
			$the_sett$elm_pretty_printer$Pretty$line);
		if (!exprs.b) {
			return _Utils_Tuple2(
				$the_sett$elm_pretty_printer$Pretty$string('()'),
				false);
		} else {
			var _v1 = A2(
				$elm$core$Tuple$mapSecond,
				$elm$core$List$any($elm$core$Basics$identity),
				$elm$core$List$unzip(
					A2(
						$elm$core$List$map,
						A3(
							$mdgriffith$elm_codegen$Internal$Write$prettyExpressionInner,
							aliases,
							$mdgriffith$elm_codegen$Internal$Write$topContext,
							A2($mdgriffith$elm_codegen$Internal$Write$decrementIndent, indent, 2)),
						$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(exprs))));
			var prettyExpressions = _v1.a;
			var alwaysBreak = _v1.b;
			return _Utils_Tuple2(
				A2(
					$mdgriffith$elm_codegen$Internal$Write$optionalGroup,
					alwaysBreak,
					$the_sett$elm_pretty_printer$Pretty$align(
						A3(
							$the_sett$elm_pretty_printer$Pretty$surround,
							open,
							close,
							A2($the_sett$elm_pretty_printer$Pretty$separators, ', ', prettyExpressions)))),
				alwaysBreak);
		}
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyDestructuring = F3(
	function (aliases, pattern, expr) {
		return A2(
			$the_sett$elm_pretty_printer$Pretty$nest,
			4,
			$the_sett$elm_pretty_printer$Pretty$lines(
				_List_fromArray(
					[
						$the_sett$elm_pretty_printer$Pretty$words(
						_List_fromArray(
							[
								A2($mdgriffith$elm_codegen$Internal$Write$prettyPattern, aliases, pattern),
								$the_sett$elm_pretty_printer$Pretty$string('=')
							])),
						A2($mdgriffith$elm_codegen$Internal$Write$prettyExpression, aliases, expr)
					])));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyInfix = function (infix_) {
	var dirToString = function (direction) {
		switch (direction) {
			case 0:
				return 'left';
			case 1:
				return 'right';
			default:
				return 'non';
		}
	};
	return $the_sett$elm_pretty_printer$Pretty$words(
		_List_fromArray(
			[
				$the_sett$elm_pretty_printer$Pretty$string('infix'),
				$the_sett$elm_pretty_printer$Pretty$string(
				dirToString(
					$mdgriffith$elm_codegen$Internal$Compiler$denode(infix_.c))),
				$the_sett$elm_pretty_printer$Pretty$string(
				$elm$core$String$fromInt(
					$mdgriffith$elm_codegen$Internal$Compiler$denode(infix_.kl))),
				$the_sett$elm_pretty_printer$Pretty$parens(
				$the_sett$elm_pretty_printer$Pretty$string(
					$mdgriffith$elm_codegen$Internal$Compiler$denode(infix_.e))),
				$the_sett$elm_pretty_printer$Pretty$string('='),
				$the_sett$elm_pretty_printer$Pretty$string(
				$mdgriffith$elm_codegen$Internal$Compiler$denode(infix_.dN))
			]));
};
var $mdgriffith$elm_codegen$Internal$Write$prettyPortDeclaration = F2(
	function (aliases, sig) {
		return $the_sett$elm_pretty_printer$Pretty$words(
			_List_fromArray(
				[
					$the_sett$elm_pretty_printer$Pretty$string('port'),
					A2($mdgriffith$elm_codegen$Internal$Write$prettySignature, aliases, sig)
				]));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyTypeAlias = F2(
	function (aliases, tAlias) {
		var typeAliasPretty = A2(
			$the_sett$elm_pretty_printer$Pretty$nest,
			4,
			A2(
				$the_sett$elm_pretty_printer$Pretty$a,
				A2(
					$mdgriffith$elm_codegen$Internal$Write$prettyTypeAnnotation,
					aliases,
					$mdgriffith$elm_codegen$Internal$Compiler$denode(tAlias.aN)),
				A2(
					$the_sett$elm_pretty_printer$Pretty$a,
					$the_sett$elm_pretty_printer$Pretty$line,
					$the_sett$elm_pretty_printer$Pretty$words(
						_List_fromArray(
							[
								$the_sett$elm_pretty_printer$Pretty$string('type alias'),
								$the_sett$elm_pretty_printer$Pretty$string(
								$mdgriffith$elm_codegen$Internal$Compiler$denode(tAlias.ao)),
								$the_sett$elm_pretty_printer$Pretty$words(
								A2(
									$elm$core$List$map,
									$the_sett$elm_pretty_printer$Pretty$string,
									$mdgriffith$elm_codegen$Internal$Compiler$denodeAll(tAlias.dP))),
								$the_sett$elm_pretty_printer$Pretty$string('=')
							])))));
		return $the_sett$elm_pretty_printer$Pretty$lines(
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_codegen$Internal$Write$prettyMaybe,
					$mdgriffith$elm_codegen$Internal$Write$prettyDocumentation,
					$mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(tAlias.aU)),
					typeAliasPretty
				]));
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyElmSyntaxDeclaration = F2(
	function (aliases, decl) {
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
				var pattern = decl.a;
				var expr = decl.b;
				return A3(
					$mdgriffith$elm_codegen$Internal$Write$prettyDestructuring,
					aliases,
					$mdgriffith$elm_codegen$Internal$Compiler$denode(pattern),
					$mdgriffith$elm_codegen$Internal$Compiler$denode(expr));
		}
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyDeclarations = F2(
	function (aliases, decls) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (decl, doc) {
					switch (decl.$) {
						case 1:
							var content = decl.a;
							return A2(
								$the_sett$elm_pretty_printer$Pretty$a,
								$the_sett$elm_pretty_printer$Pretty$line,
								A2(
									$the_sett$elm_pretty_printer$Pretty$a,
									$the_sett$elm_pretty_printer$Pretty$line,
									A2(
										$the_sett$elm_pretty_printer$Pretty$a,
										$the_sett$elm_pretty_printer$Pretty$string(content + '\n'),
										doc)));
						case 2:
							var source = decl.a;
							return A2(
								$the_sett$elm_pretty_printer$Pretty$a,
								$the_sett$elm_pretty_printer$Pretty$line,
								A2(
									$the_sett$elm_pretty_printer$Pretty$a,
									$the_sett$elm_pretty_printer$Pretty$line,
									A2(
										$the_sett$elm_pretty_printer$Pretty$a,
										$the_sett$elm_pretty_printer$Pretty$line,
										A2(
											$the_sett$elm_pretty_printer$Pretty$a,
											$the_sett$elm_pretty_printer$Pretty$string(source),
											doc))));
						default:
							var innerDecl = decl.a;
							return A2(
								$the_sett$elm_pretty_printer$Pretty$a,
								$the_sett$elm_pretty_printer$Pretty$line,
								A2(
									$the_sett$elm_pretty_printer$Pretty$a,
									$the_sett$elm_pretty_printer$Pretty$line,
									A2(
										$the_sett$elm_pretty_printer$Pretty$a,
										$the_sett$elm_pretty_printer$Pretty$line,
										A2(
											$the_sett$elm_pretty_printer$Pretty$a,
											A2($mdgriffith$elm_codegen$Internal$Write$prettyElmSyntaxDeclaration, aliases, innerDecl),
											doc))));
					}
				}),
			$the_sett$elm_pretty_printer$Pretty$empty,
			decls);
	});
var $mdgriffith$elm_codegen$Internal$Comments$delimeters = function (doc) {
	return A2(
		$the_sett$elm_pretty_printer$Pretty$a,
		$the_sett$elm_pretty_printer$Pretty$string('-}'),
		A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			$the_sett$elm_pretty_printer$Pretty$line,
			A2(
				$the_sett$elm_pretty_printer$Pretty$a,
				doc,
				$the_sett$elm_pretty_printer$Pretty$string('{-| '))));
};
var $mdgriffith$elm_codegen$Internal$Comments$getParts = function (_v0) {
	var parts = _v0;
	return $elm$core$List$reverse(parts);
};
var $mdgriffith$elm_codegen$Internal$Comments$DocTags = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_codegen$Internal$Comments$fitAndSplit = F2(
	function (width, tags) {
		if (!tags.b) {
			return _List_Nil;
		} else {
			var t = tags.a;
			var ts = tags.b;
			var _v1 = A3(
				$elm$core$List$foldl,
				F2(
					function (tag, _v2) {
						var allSplits = _v2.a;
						var curSplit = _v2.b;
						var remaining = _v2.c;
						return (_Utils_cmp(
							$elm$core$String$length(tag),
							remaining) < 1) ? _Utils_Tuple3(
							allSplits,
							A2($elm$core$List$cons, tag, curSplit),
							remaining - $elm$core$String$length(tag)) : _Utils_Tuple3(
							_Utils_ap(
								allSplits,
								_List_fromArray(
									[
										$elm$core$List$reverse(curSplit)
									])),
							_List_fromArray(
								[tag]),
							width - $elm$core$String$length(tag));
					}),
				_Utils_Tuple3(
					_List_Nil,
					_List_fromArray(
						[t]),
					width - $elm$core$String$length(t)),
				ts);
			var splitsExceptLast = _v1.a;
			var lastSplit = _v1.b;
			return _Utils_ap(
				splitsExceptLast,
				_List_fromArray(
					[
						$elm$core$List$reverse(lastSplit)
					]));
		}
	});
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $mdgriffith$elm_codegen$Internal$Comments$mergeDocTags = function (innerParts) {
	var _v0 = A3(
		$elm$core$List$foldr,
		F2(
			function (part, _v1) {
				var accum = _v1.a;
				var context = _v1.b;
				if (context.$ === 1) {
					if (part.$ === 2) {
						var tags = part.a;
						return _Utils_Tuple2(
							accum,
							$elm$core$Maybe$Just(tags));
					} else {
						var otherPart = part;
						return _Utils_Tuple2(
							A2($elm$core$List$cons, otherPart, accum),
							$elm$core$Maybe$Nothing);
					}
				} else {
					var contextTags = context.a;
					if (part.$ === 2) {
						var tags = part.a;
						return _Utils_Tuple2(
							accum,
							$elm$core$Maybe$Just(
								_Utils_ap(contextTags, tags)));
					} else {
						var otherPart = part;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								otherPart,
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_codegen$Internal$Comments$DocTags(
										$elm$core$List$sort(contextTags)),
									accum)),
							$elm$core$Maybe$Nothing);
					}
				}
			}),
		_Utils_Tuple2(_List_Nil, $elm$core$Maybe$Nothing),
		innerParts);
	var partsExceptMaybeFirst = _v0.a;
	var maybeFirstPart = _v0.b;
	if (maybeFirstPart.$ === 1) {
		return partsExceptMaybeFirst;
	} else {
		var tags = maybeFirstPart.a;
		return A2(
			$elm$core$List$cons,
			$mdgriffith$elm_codegen$Internal$Comments$DocTags(
				$elm$core$List$sort(tags)),
			partsExceptMaybeFirst);
	}
};
var $mdgriffith$elm_codegen$Internal$Comments$layoutTags = F2(
	function (width, parts) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (part, _v0) {
					var accumParts = _v0.a;
					var accumDocTags = _v0.b;
					if (part.$ === 2) {
						var tags = part.a;
						var splits = A2($mdgriffith$elm_codegen$Internal$Comments$fitAndSplit, width, tags);
						return _Utils_Tuple2(
							_Utils_ap(
								A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Comments$DocTags, splits),
								accumParts),
							_Utils_ap(accumDocTags, splits));
					} else {
						var otherPart = part;
						return _Utils_Tuple2(
							A2($elm$core$List$cons, otherPart, accumParts),
							accumDocTags);
					}
				}),
			_Utils_Tuple2(_List_Nil, _List_Nil),
			$mdgriffith$elm_codegen$Internal$Comments$mergeDocTags(parts));
	});
var $the_sett$elm_pretty_printer$Internals$NLine = F3(
	function (a, b, c) {
		return {$: 2, a: a, b: b, c: c};
	});
var $the_sett$elm_pretty_printer$Internals$NNil = {$: 0};
var $the_sett$elm_pretty_printer$Internals$NText = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $the_sett$elm_pretty_printer$Internals$fits = F2(
	function (w, normal) {
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
						var $temp$w = w - $elm$core$String$length(text),
							$temp$normal = innerNormal(0);
						w = $temp$w;
						normal = $temp$normal;
						continue fits;
					default:
						return true;
				}
			}
		}
	});
var $the_sett$elm_pretty_printer$Internals$better = F4(
	function (w, k, doc, doc2Fn) {
		return A2($the_sett$elm_pretty_printer$Internals$fits, w - k, doc) ? doc : doc2Fn(0);
	});
var $the_sett$elm_pretty_printer$Internals$best = F3(
	function (width, startCol, x) {
		var be = F3(
			function (w, k, docs) {
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
								var $temp$w = w,
									$temp$k = k,
									$temp$docs = ds;
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
								var $temp$w = w,
									$temp$k = k,
									$temp$docs = A2(
									$elm$core$List$cons,
									_Utils_Tuple2(
										i,
										doc(0)),
									A2(
										$elm$core$List$cons,
										_Utils_Tuple2(
											i,
											doc2(0)),
										ds));
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
								var $temp$w = w,
									$temp$k = k,
									$temp$docs = A2(
									$elm$core$List$cons,
									_Utils_Tuple2(
										i + j,
										doc(0)),
									ds);
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
								return A3(
									$the_sett$elm_pretty_printer$Internals$NText,
									text,
									function (_v9) {
										return A3(
											be,
											w,
											k + $elm$core$String$length(text),
											ds);
									},
									maybeTag);
							case 4:
								var _v10 = docs.a;
								var i = _v10.a;
								var _v11 = _v10.b;
								var vsep = _v11.b;
								var ds = docs.b;
								return A3(
									$the_sett$elm_pretty_printer$Internals$NLine,
									i,
									vsep,
									function (_v12) {
										return A3(
											be,
											w,
											i + $elm$core$String$length(vsep),
											ds);
									});
							case 5:
								var _v13 = docs.a;
								var i = _v13.a;
								var _v14 = _v13.b;
								var doc = _v14.a;
								var doc2 = _v14.b;
								var ds = docs.b;
								return A4(
									$the_sett$elm_pretty_printer$Internals$better,
									w,
									k,
									A3(
										be,
										w,
										k,
										A2(
											$elm$core$List$cons,
											_Utils_Tuple2(i, doc),
											ds)),
									function (_v15) {
										return A3(
											be,
											w,
											k,
											A2(
												$elm$core$List$cons,
												_Utils_Tuple2(i, doc2),
												ds));
									});
							case 6:
								var _v16 = docs.a;
								var i = _v16.a;
								var fn = _v16.b.a;
								var ds = docs.b;
								var $temp$w = w,
									$temp$k = k,
									$temp$docs = A2(
									$elm$core$List$cons,
									_Utils_Tuple2(
										i,
										fn(i)),
									ds);
								w = $temp$w;
								k = $temp$k;
								docs = $temp$docs;
								continue be;
							default:
								var _v17 = docs.a;
								var i = _v17.a;
								var fn = _v17.b.a;
								var ds = docs.b;
								var $temp$w = w,
									$temp$k = k,
									$temp$docs = A2(
									$elm$core$List$cons,
									_Utils_Tuple2(
										i,
										fn(k)),
									ds);
								w = $temp$w;
								k = $temp$k;
								docs = $temp$docs;
								continue be;
						}
					}
				}
			});
		return A3(
			be,
			width,
			startCol,
			_List_fromArray(
				[
					_Utils_Tuple2(0, x)
				]));
	});
var $the_sett$elm_pretty_printer$Internals$layout = function (normal) {
	var layoutInner = F2(
		function (normal2, acc) {
			layoutInner:
			while (true) {
				switch (normal2.$) {
					case 0:
						return acc;
					case 1:
						var text = normal2.a;
						var innerNormal = normal2.b;
						var maybeTag = normal2.c;
						var $temp$normal2 = innerNormal(0),
							$temp$acc = A2($elm$core$List$cons, text, acc);
						normal2 = $temp$normal2;
						acc = $temp$acc;
						continue layoutInner;
					default:
						var i = normal2.a;
						var sep = normal2.b;
						var innerNormal = normal2.c;
						var norm = innerNormal(0);
						if (norm.$ === 2) {
							var $temp$normal2 = innerNormal(0),
								$temp$acc = A2($elm$core$List$cons, '\n' + sep, acc);
							normal2 = $temp$normal2;
							acc = $temp$acc;
							continue layoutInner;
						} else {
							var $temp$normal2 = innerNormal(0),
								$temp$acc = A2(
								$elm$core$List$cons,
								'\n' + (A2($the_sett$elm_pretty_printer$Internals$copy, i, ' ') + sep),
								acc);
							normal2 = $temp$normal2;
							acc = $temp$acc;
							continue layoutInner;
						}
				}
			}
		});
	return $elm$core$String$concat(
		$elm$core$List$reverse(
			A2(layoutInner, normal, _List_Nil)));
};
var $the_sett$elm_pretty_printer$Pretty$pretty = F2(
	function (w, doc) {
		return $the_sett$elm_pretty_printer$Internals$layout(
			A3($the_sett$elm_pretty_printer$Internals$best, w, 0, doc));
	});
var $mdgriffith$elm_codegen$Internal$Comments$prettyCode = function (val) {
	return A2(
		$the_sett$elm_pretty_printer$Pretty$indent,
		4,
		$the_sett$elm_pretty_printer$Pretty$string(val));
};
var $mdgriffith$elm_codegen$Internal$Comments$prettyMarkdown = function (val) {
	return $the_sett$elm_pretty_printer$Pretty$string(val);
};
var $mdgriffith$elm_codegen$Internal$Comments$prettyTags = function (tags) {
	return $the_sett$elm_pretty_printer$Pretty$words(
		_List_fromArray(
			[
				$the_sett$elm_pretty_printer$Pretty$string('@docs'),
				A2(
				$the_sett$elm_pretty_printer$Pretty$join,
				$the_sett$elm_pretty_printer$Pretty$string(', '),
				A2($elm$core$List$map, $the_sett$elm_pretty_printer$Pretty$string, tags))
			]));
};
var $mdgriffith$elm_codegen$Internal$Comments$prettyCommentPart = function (part) {
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
var $mdgriffith$elm_codegen$Internal$Comments$prettyFileComment = F2(
	function (width, comment) {
		var _v0 = A2(
			$mdgriffith$elm_codegen$Internal$Comments$layoutTags,
			width,
			$mdgriffith$elm_codegen$Internal$Comments$getParts(comment));
		var parts = _v0.a;
		var splits = _v0.b;
		return _Utils_Tuple2(
			A2(
				$the_sett$elm_pretty_printer$Pretty$pretty,
				width,
				$mdgriffith$elm_codegen$Internal$Comments$delimeters(
					$the_sett$elm_pretty_printer$Pretty$lines(
						A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Comments$prettyCommentPart, parts)))),
			splits);
	});
var $mdgriffith$elm_codegen$Internal$Write$prettyDefaultModuleData = function (moduleData) {
	return $the_sett$elm_pretty_printer$Pretty$words(
		_List_fromArray(
			[
				$the_sett$elm_pretty_printer$Pretty$string('module'),
				$mdgriffith$elm_codegen$Internal$Write$prettyModuleName(
				$mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.a1)),
				$mdgriffith$elm_codegen$Internal$Write$prettyExposing(
				$mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.di))
			]));
};
var $mdgriffith$elm_codegen$Internal$Write$prettyEffectModuleData = function (moduleData) {
	var prettyCmdAndSub = F2(
		function (maybeCmd, maybeSub) {
			var _v0 = _Utils_Tuple2(maybeCmd, maybeSub);
			if (!_v0.a.$) {
				if (!_v0.b.$) {
					var cmdName = _v0.a.a;
					var subName = _v0.b.a;
					return $elm$core$Maybe$Just(
						$the_sett$elm_pretty_printer$Pretty$words(
							_List_fromArray(
								[
									$the_sett$elm_pretty_printer$Pretty$string('where { command ='),
									$the_sett$elm_pretty_printer$Pretty$string(cmdName),
									$the_sett$elm_pretty_printer$Pretty$string(','),
									$the_sett$elm_pretty_printer$Pretty$string('subscription ='),
									$the_sett$elm_pretty_printer$Pretty$string(subName),
									$the_sett$elm_pretty_printer$Pretty$string('}')
								])));
				} else {
					var cmdName = _v0.a.a;
					var _v3 = _v0.b;
					return $elm$core$Maybe$Just(
						$the_sett$elm_pretty_printer$Pretty$words(
							_List_fromArray(
								[
									$the_sett$elm_pretty_printer$Pretty$string('where { command ='),
									$the_sett$elm_pretty_printer$Pretty$string(cmdName),
									$the_sett$elm_pretty_printer$Pretty$string('}')
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
					return $elm$core$Maybe$Just(
						$the_sett$elm_pretty_printer$Pretty$words(
							_List_fromArray(
								[
									$the_sett$elm_pretty_printer$Pretty$string('where { subscription ='),
									$the_sett$elm_pretty_printer$Pretty$string(subName),
									$the_sett$elm_pretty_printer$Pretty$string('}')
								])));
				}
			}
		});
	return $the_sett$elm_pretty_printer$Pretty$words(
		_List_fromArray(
			[
				$the_sett$elm_pretty_printer$Pretty$string('effect module'),
				$mdgriffith$elm_codegen$Internal$Write$prettyModuleName(
				$mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.a1)),
				A2(
				$mdgriffith$elm_codegen$Internal$Write$prettyMaybe,
				$elm$core$Basics$identity,
				A2(
					prettyCmdAndSub,
					$mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(moduleData.i8),
					$mdgriffith$elm_codegen$Internal$Compiler$denodeMaybe(moduleData.kQ))),
				$mdgriffith$elm_codegen$Internal$Write$prettyExposing(
				$mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.di))
			]));
};
var $mdgriffith$elm_codegen$Internal$Write$prettyPortModuleData = function (moduleData) {
	return $the_sett$elm_pretty_printer$Pretty$words(
		_List_fromArray(
			[
				$the_sett$elm_pretty_printer$Pretty$string('port module'),
				$mdgriffith$elm_codegen$Internal$Write$prettyModuleName(
				$mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.a1)),
				$mdgriffith$elm_codegen$Internal$Write$prettyExposing(
				$mdgriffith$elm_codegen$Internal$Compiler$denode(moduleData.di))
			]));
};
var $mdgriffith$elm_codegen$Internal$Write$prettyModule = function (mod) {
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
var $mdgriffith$elm_codegen$Internal$Write$prepareLayout = F2(
	function (width, file) {
		return A2(
			$the_sett$elm_pretty_printer$Pretty$a,
			A2($mdgriffith$elm_codegen$Internal$Write$prettyDeclarations, file.iD, file.ai),
			A2(
				$the_sett$elm_pretty_printer$Pretty$a,
				$mdgriffith$elm_codegen$Internal$Write$importsPretty(file.a),
				function (doc) {
					var _v0 = file.i9;
					if (_v0.$ === 1) {
						return doc;
					} else {
						var fileComment = _v0.a;
						var _v1 = A2($mdgriffith$elm_codegen$Internal$Comments$prettyFileComment, width, fileComment);
						var fileCommentStr = _v1.a;
						var innerTags = _v1.b;
						return A2(
							$the_sett$elm_pretty_printer$Pretty$a,
							$the_sett$elm_pretty_printer$Pretty$line,
							A2(
								$the_sett$elm_pretty_printer$Pretty$a,
								$mdgriffith$elm_codegen$Internal$Write$prettyComments(
									_List_fromArray(
										[fileCommentStr])),
								doc));
					}
				}(
					A2(
						$the_sett$elm_pretty_printer$Pretty$a,
						$the_sett$elm_pretty_printer$Pretty$line,
						A2(
							$the_sett$elm_pretty_printer$Pretty$a,
							$the_sett$elm_pretty_printer$Pretty$line,
							$mdgriffith$elm_codegen$Internal$Write$prettyModule(file.fh))))));
	});
var $mdgriffith$elm_codegen$Internal$Write$pretty = F2(
	function (width, file) {
		return A2(
			$the_sett$elm_pretty_printer$Pretty$pretty,
			width,
			A2($mdgriffith$elm_codegen$Internal$Write$prepareLayout, width, file));
	});
var $mdgriffith$elm_codegen$Internal$Write$write = $mdgriffith$elm_codegen$Internal$Write$pretty(80);
var $mdgriffith$elm_codegen$Internal$Render$render = F2(
	function (toDocComment, fileDetails) {
		var rendered = A3(
			$elm$core$List$foldl,
			F2(
				function (decl, gathered) {
					switch (decl.$) {
						case 1:
							var comm = decl.a;
							return _Utils_update(
								gathered,
								{
									ai: A2(
										$elm$core$List$cons,
										$mdgriffith$elm_codegen$Internal$Compiler$RenderedComment(comm),
										gathered.ai)
								});
						case 2:
							var block = decl.a;
							return _Utils_update(
								gathered,
								{
									ai: A2(
										$elm$core$List$cons,
										$mdgriffith$elm_codegen$Internal$Compiler$RenderedBlock(block),
										gathered.ai)
								});
						default:
							var decDetails = decl.a;
							var result = decDetails.ac(fileDetails.jK);
							return {
								ai: A2(
									$elm$core$List$cons,
									$mdgriffith$elm_codegen$Internal$Compiler$RenderedDecl(
										A2($mdgriffith$elm_codegen$Internal$Render$addDocs, decDetails.aT, result.jf)),
									gathered.ai),
								dh: A3($mdgriffith$elm_codegen$Internal$Render$addExposed, decDetails.dh, result.jf, gathered.dh),
								V: function () {
									var _v5 = decDetails.dh;
									if (!_v5.$) {
										return gathered.V;
									} else {
										var details = _v5.a;
										return A2(
											$elm$core$List$cons,
											_Utils_Tuple2(details.dV, decDetails.ao),
											gathered.V);
									}
								}(),
								ak: function () {
									if (gathered.ak) {
										return gathered.ak;
									} else {
										var _v6 = result.jf;
										if (_v6.$ === 3) {
											return true;
										} else {
											return false;
										}
									}
								}(),
								a: _Utils_ap(
									result.T,
									_Utils_ap(decDetails.a, gathered.a)),
								io: function () {
									var _v7 = result.k0;
									if (_v7.$ === 1) {
										return gathered.io;
									} else {
										var warn = _v7.a;
										return A2($elm$core$List$cons, warn, gathered.io);
									}
								}()
							};
					}
				}),
			{ai: _List_Nil, dh: _List_Nil, V: _List_Nil, ak: false, a: _List_Nil, io: _List_Nil},
			fileDetails.ai);
		var body = $mdgriffith$elm_codegen$Internal$Write$write(
			{
				iD: fileDetails.iD,
				i9: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Internal$Comments$addPart,
						$mdgriffith$elm_codegen$Internal$Comments$emptyComment,
						$mdgriffith$elm_codegen$Internal$Comments$Markdown(
							function () {
								var _v0 = rendered.V;
								if (!_v0.b) {
									return '';
								} else {
									return '\n' + A2(
										$elm$core$String$join,
										'\n\n',
										toDocComment(
											$mdgriffith$elm_codegen$Internal$Render$groupExposing(
												A2(
													$elm$core$List$sortBy,
													function (_v1) {
														var group = _v1.a;
														if (group.$ === 1) {
															return 'zzzzzzzzz';
														} else {
															var name = group.a;
															return name;
														}
													},
													rendered.V))));
								}
							}()))),
				ai: $elm$core$List$reverse(rendered.ai),
				a: A2(
					$elm$core$List$filterMap,
					$mdgriffith$elm_codegen$Internal$Compiler$makeImport(fileDetails.iD),
					$mdgriffith$elm_codegen$Internal$Render$dedupImports(rendered.a)),
				fh: (rendered.ak ? $stil4m$elm_syntax$Elm$Syntax$Module$PortModule : $stil4m$elm_syntax$Elm$Syntax$Module$NormalModule)(
					{
						di: function () {
							var _v3 = rendered.dh;
							if (!_v3.b) {
								return $mdgriffith$elm_codegen$Internal$Compiler$nodify(
									$stil4m$elm_syntax$Elm$Syntax$Exposing$All($stil4m$elm_syntax$Elm$Syntax$Range$emptyRange));
							} else {
								return $mdgriffith$elm_codegen$Internal$Compiler$nodify(
									$stil4m$elm_syntax$Elm$Syntax$Exposing$Explicit(
										$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(rendered.dh)));
							}
						}(),
						a1: $mdgriffith$elm_codegen$Internal$Compiler$nodify(fileDetails.a1)
					})
			});
		return {
			cj: body,
			f4: A2($elm$core$String$join, '/', fileDetails.a1) + '.elm',
			io: rendered.io
		};
	});
var $mdgriffith$elm_codegen$Elm$docs = function (group) {
	var _v0 = group.dV;
	if (_v0.$ === 1) {
		return '@docs ' + A2($elm$core$String$join, ', ', group.j0);
	} else {
		var groupName = _v0.a;
		return '## ' + (groupName + ('\n\n@docs ' + A2($elm$core$String$join, ', ', group.j0)));
	}
};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_codegen$Elm$renderStandardComment = function (groups) {
	return $elm$core$List$isEmpty(groups) ? _List_Nil : A2($elm$core$List$map, $mdgriffith$elm_codegen$Elm$docs, groups);
};
var $mdgriffith$elm_codegen$Internal$Index$Index = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_codegen$Internal$Index$startIndex = A4($mdgriffith$elm_codegen$Internal$Index$Index, 0, _List_Nil, $elm$core$Set$empty, true);
var $mdgriffith$elm_codegen$Elm$file = F2(
	function (mod, decs) {
		return A2(
			$mdgriffith$elm_codegen$Internal$Render$render,
			$mdgriffith$elm_codegen$Elm$renderStandardComment,
			{iD: _List_Nil, ai: decs, jK: $mdgriffith$elm_codegen$Internal$Index$startIndex, a1: mod});
	});
var $mdgriffith$elm_codegen$Internal$Format$formatType = function (str) {
	return _Utils_ap(
		$elm$core$String$toUpper(
			A2($elm$core$String$left, 1, str)),
		A2($elm$core$String$dropLeft, 1, str));
};
var $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports = function (_v0) {
	var details = _v0;
	return details.a;
};
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$uniqueHelp = F4(
	function (f, existing, remaining, accumulator) {
		uniqueHelp:
		while (true) {
			if (!remaining.b) {
				return $elm$core$List$reverse(accumulator);
			} else {
				var first = remaining.a;
				var rest = remaining.b;
				var computedFirst = f(first);
				if (A2($elm$core$List$member, computedFirst, existing)) {
					var $temp$f = f,
						$temp$existing = existing,
						$temp$remaining = rest,
						$temp$accumulator = accumulator;
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				} else {
					var $temp$f = f,
						$temp$existing = A2($elm$core$List$cons, computedFirst, existing),
						$temp$remaining = rest,
						$temp$accumulator = A2($elm$core$List$cons, first, accumulator);
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				}
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$unique = function (list) {
	return A4($mdgriffith$elm_codegen$Internal$Compiler$uniqueHelp, $elm$core$Basics$identity, _List_Nil, list, _List_Nil);
};
var $mdgriffith$elm_codegen$Internal$Compiler$getGenerics = function (_v0) {
	var details = _v0;
	return $mdgriffith$elm_codegen$Internal$Compiler$unique(
		$mdgriffith$elm_codegen$Internal$Compiler$getGenericsHelper(details.bx));
};
var $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation = function (_v0) {
	var details = _v0;
	return details.bx;
};
var $mdgriffith$elm_codegen$Elm$alias = F2(
	function (name, innerAnnotation) {
		return $mdgriffith$elm_codegen$Internal$Compiler$Declaration(
			{
				aT: $elm$core$Maybe$Nothing,
				dh: $mdgriffith$elm_codegen$Internal$Compiler$NotExposed,
				a: $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(innerAnnotation),
				ao: name,
				ac: function (index) {
					return {
						T: _List_Nil,
						jf: $stil4m$elm_syntax$Elm$Syntax$Declaration$AliasDeclaration(
							{
								aU: $elm$core$Maybe$Nothing,
								dP: A2(
									$elm$core$List$map,
									$mdgriffith$elm_codegen$Internal$Compiler$nodify,
									$mdgriffith$elm_codegen$Internal$Compiler$getGenerics(innerAnnotation)),
								ao: $mdgriffith$elm_codegen$Internal$Compiler$nodify(
									$mdgriffith$elm_codegen$Internal$Format$formatType(name)),
								aN: $mdgriffith$elm_codegen$Internal$Compiler$nodify(
									$mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(innerAnnotation))
							}),
						k0: $elm$core$Maybe$Nothing
					};
				}
			});
	});
var $mdgriffith$elm_codegen$Internal$Compiler$Annotation = $elm$core$Basics$identity;
var $mdgriffith$elm_codegen$Internal$Compiler$formatAliasKey = F2(
	function (mod, name) {
		return A2($elm$core$String$join, '.', mod) + ('.' + name);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$addAlias = F4(
	function (mod, name, ann, aliasCache) {
		var annDetails = ann;
		return A3(
			$elm$core$Dict$insert,
			A2($mdgriffith$elm_codegen$Internal$Compiler$formatAliasKey, mod, name),
			{
				hu: annDetails.bx,
				be: $mdgriffith$elm_codegen$Internal$Compiler$getGenerics(ann)
			},
			aliasCache);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$getAliases = function (_v0) {
	var ann = _v0;
	return ann.iD;
};
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$mergeAliases = $elm$core$Dict$union;
var $mdgriffith$elm_codegen$Elm$Annotation$alias = F4(
	function (mod, name, vars, target) {
		return {
			iD: A4(
				$mdgriffith$elm_codegen$Internal$Compiler$addAlias,
				mod,
				name,
				target,
				A3(
					$elm$core$List$foldl,
					F2(
						function (ann, aliases) {
							return A2(
								$mdgriffith$elm_codegen$Internal$Compiler$mergeAliases,
								$mdgriffith$elm_codegen$Internal$Compiler$getAliases(ann),
								aliases);
						}),
					$mdgriffith$elm_codegen$Internal$Compiler$getAliases(target),
					vars)),
			bx: A2(
				$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
				$mdgriffith$elm_codegen$Internal$Compiler$nodify(
					_Utils_Tuple2(
						mod,
						$mdgriffith$elm_codegen$Internal$Format$formatType(name))),
				A2(
					$elm$core$List$map,
					A2($elm$core$Basics$composeL, $mdgriffith$elm_codegen$Internal$Compiler$nodify, $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation),
					vars)),
			a: function () {
				if (!mod.b) {
					return A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, vars);
				} else {
					return _Utils_ap(
						_List_fromArray(
							[mod]),
						A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, vars));
				}
			}()
		};
	});
var $author$project$Gen$App$moduleName_ = _List_fromArray(
	['App']);
var $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases = $elm$core$Dict$empty;
var $mdgriffith$elm_codegen$Elm$Annotation$getAliases = function (_v0) {
	var ann = _v0;
	return ann.iD;
};
var $mdgriffith$elm_codegen$Elm$Annotation$namedWith = F3(
	function (mod, name, args) {
		return {
			iD: A3(
				$elm$core$List$foldl,
				F2(
					function (ann, aliases) {
						return A2(
							$mdgriffith$elm_codegen$Internal$Compiler$mergeAliases,
							$mdgriffith$elm_codegen$Elm$Annotation$getAliases(ann),
							aliases);
					}),
				$mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
				args),
			bx: A2(
				$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
				$mdgriffith$elm_codegen$Internal$Compiler$nodify(
					_Utils_Tuple2(
						mod,
						$mdgriffith$elm_codegen$Internal$Format$formatType(name))),
				$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
					A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation, args))),
			a: A2(
				$elm$core$List$cons,
				mod,
				A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, args))
		};
	});
var $mdgriffith$elm_codegen$Elm$Annotation$var = function (a) {
	return {
		iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
		bx: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(
			$mdgriffith$elm_codegen$Internal$Format$formatValue(a)),
		a: _List_Nil
	};
};
var $author$project$Gen$App$annotation_ = {
	km: F3(
		function (progArg0, progArg1, progArg2) {
			return A4(
				$mdgriffith$elm_codegen$Elm$Annotation$alias,
				$author$project$Gen$App$moduleName_,
				'Prog',
				_List_fromArray(
					[progArg0, progArg1, progArg2]),
				A3(
					$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
					_List_Nil,
					'Program',
					_List_fromArray(
						[
							$mdgriffith$elm_codegen$Elm$Annotation$var('flags'),
							A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_Nil,
							'Model',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('model')
								])),
							A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_Nil,
							'Msg',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								]))
						])));
		})
};
var $mdgriffith$elm_codegen$Elm$Annotation$named = F2(
	function (mod, name) {
		return {
			iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
			bx: A2(
				$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
				$mdgriffith$elm_codegen$Internal$Compiler$nodify(
					_Utils_Tuple2(
						mod,
						$mdgriffith$elm_codegen$Internal$Format$formatType(name))),
				_List_Nil),
			a: function () {
				if (!mod.b) {
					return _List_Nil;
				} else {
					return _List_fromArray(
						[mod]);
				}
			}()
		};
	});
var $author$project$Interactive$appTypes = {
	_: A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, 'Model'),
	x: A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, 'Msg')
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Application = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$FunctionAppliedToTooManyArgs = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_codegen$Internal$Compiler$containsFieldByName = F2(
	function (_v0, _v2) {
		var _v1 = _v0.a;
		var oneName = _v1.b;
		var _v3 = _v2.a;
		var twoName = _v3.b;
		return _Utils_eq(oneName, twoName);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$mergeFieldLists = F2(
	function (fieldOne, fieldTwo) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_new, existing) {
					var newField = _new.b;
					return A2(
						$elm$core$List$any,
						A2(
							$elm$core$Basics$composeL,
							$mdgriffith$elm_codegen$Internal$Compiler$containsFieldByName(newField),
							$mdgriffith$elm_codegen$Internal$Compiler$denode),
						existing) ? existing : A2($elm$core$List$cons, _new, existing);
				}),
			fieldOne,
			fieldTwo);
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
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
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
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
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
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
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
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
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
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
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
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
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
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
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
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
				if (_v4.$ === -1) {
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
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
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
						if (_v7.$ === -1) {
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
		if ((_v0.$ === -1) && (!_v0.a)) {
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
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$addInference = F3(
	function (key, value, infs) {
		return A3(
			$elm$core$Dict$update,
			key,
			function (maybeValue) {
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
							var _v5 = value.a;
							var existingRange = _v5.a;
							var existingRecordName = _v5.b;
							var _v6 = value.b;
							var existingFieldRange = _v6.a;
							var existingFields = _v6.b;
							return $elm$core$Maybe$Just(
								A2(
									$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord,
									A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, range, recordName),
									A2(
										$stil4m$elm_syntax$Elm$Syntax$Node$Node,
										fieldRange,
										A2($mdgriffith$elm_codegen$Internal$Compiler$mergeFieldLists, fields, existingFields))));
						} else {
							return maybeValue;
						}
					} else {
						var existing = maybeValue.a;
						return $elm$core$Maybe$Just(existing);
					}
				}
			},
			infs);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$makeFunctionReversedHelper = F2(
	function (last, reversedArgs) {
		makeFunctionReversedHelper:
		while (true) {
			if (!reversedArgs.b) {
				return last;
			} else {
				if (!reversedArgs.b.b) {
					var penUlt = reversedArgs.a;
					return A2(
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
						A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, penUlt),
						A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, last));
				} else {
					var penUlt = reversedArgs.a;
					var remain = reversedArgs.b;
					var $temp$last = A2(
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
						A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, penUlt),
						A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, last)),
						$temp$reversedArgs = remain;
					last = $temp$last;
					reversedArgs = $temp$reversedArgs;
					continue makeFunctionReversedHelper;
				}
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$makeFunction = F2(
	function (result, args) {
		return A2(
			$mdgriffith$elm_codegen$Internal$Compiler$makeFunctionReversedHelper,
			result,
			$elm$core$List$reverse(args));
	});
var $mdgriffith$elm_codegen$Internal$Compiler$MismatchedTypeVariables = {$: 4};
var $mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify = F2(
	function (a, b) {
		return {$: 14, a: a, b: b};
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $mdgriffith$elm_codegen$Internal$Compiler$getAlias = F2(
	function (_v0, cache) {
		var _v1 = _v0.b;
		var modName = _v1.a;
		var name = _v1.b;
		return A2(
			$elm$core$Dict$get,
			A2($mdgriffith$elm_codegen$Internal$Compiler$formatAliasKey, modName, name),
			cache);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$CouldNotFindField = function (a) {
	return {$: 7, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$getField = F4(
	function (name, val, fields, captured) {
		getField:
		while (true) {
			if (!fields.b) {
				return $elm$core$Result$Err(
					$mdgriffith$elm_codegen$Internal$Compiler$CouldNotFindField(
						{
							jo: A2(
								$elm$core$List$map,
								A2(
									$elm$core$Basics$composeR,
									$mdgriffith$elm_codegen$Internal$Compiler$denode,
									A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $mdgriffith$elm_codegen$Internal$Compiler$denode)),
								captured),
							w: name
						}));
			} else {
				var top = fields.a;
				var remain = fields.b;
				var _v1 = $mdgriffith$elm_codegen$Internal$Compiler$denode(top);
				var topFieldName = _v1.a;
				var topFieldVal = _v1.b;
				var topName = $mdgriffith$elm_codegen$Internal$Compiler$denode(topFieldName);
				var topVal = $mdgriffith$elm_codegen$Internal$Compiler$denode(topFieldVal);
				if (_Utils_eq(topName, name)) {
					return $elm$core$Result$Ok(
						_Utils_Tuple2(
							topVal,
							_Utils_ap(captured, remain)));
				} else {
					var $temp$name = name,
						$temp$val = val,
						$temp$fields = remain,
						$temp$captured = A2($elm$core$List$cons, top, captured);
					name = $temp$name;
					val = $temp$val;
					fields = $temp$fields;
					captured = $temp$captured;
					continue getField;
				}
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$unifiable = F4(
	function (aliases, vars, one, two) {
		unifiable:
		while (true) {
			switch (one.$) {
				case 0:
					var varName = one.a;
					var _v21 = A2($elm$core$Dict$get, varName, vars);
					if (_v21.$ === 1) {
						if (!two.$) {
							var varNameB = two.a;
							return _Utils_eq(varNameB, varName) ? _Utils_Tuple2(
								vars,
								$elm$core$Result$Ok(one)) : _Utils_Tuple2(
								A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, varName, two, vars),
								$elm$core$Result$Ok(two));
						} else {
							return _Utils_Tuple2(
								A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, varName, two, vars),
								$elm$core$Result$Ok(two));
						}
					} else {
						var found = _v21.a;
						if (!two.$) {
							var varNameB = two.a;
							if (_Utils_eq(varNameB, varName)) {
								return _Utils_Tuple2(
									vars,
									$elm$core$Result$Ok(one));
							} else {
								var _v24 = A2($elm$core$Dict$get, varNameB, vars);
								if (_v24.$ === 1) {
									return _Utils_Tuple2(
										A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, varNameB, found, vars),
										$elm$core$Result$Ok(two));
								} else {
									var foundTwo = _v24.a;
									var $temp$aliases = aliases,
										$temp$vars = vars,
										$temp$one = found,
										$temp$two = foundTwo;
									aliases = $temp$aliases;
									vars = $temp$vars;
									one = $temp$one;
									two = $temp$two;
									continue unifiable;
								}
							}
						} else {
							var $temp$aliases = aliases,
								$temp$vars = vars,
								$temp$one = found,
								$temp$two = two;
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
							if (_Utils_eq(
								$mdgriffith$elm_codegen$Internal$Compiler$denode(oneName),
								$mdgriffith$elm_codegen$Internal$Compiler$denode(twoName))) {
								var _v26 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableLists, aliases, vars, oneVars, twoContents, _List_Nil);
								if (!_v26.b.$) {
									var newVars = _v26.a;
									var unifiedContent = _v26.b.a;
									return _Utils_Tuple2(
										newVars,
										$elm$core$Result$Ok(
											A2($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed, twoName, unifiedContent)));
								} else {
									var newVars = _v26.a;
									var err = _v26.b.a;
									return _Utils_Tuple2(
										newVars,
										$elm$core$Result$Err(err));
								}
							} else {
								return _Utils_Tuple2(
									vars,
									$elm$core$Result$Err(
										A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
							}
						case 0:
							var b = two.a;
							return _Utils_Tuple2(
								A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars),
								$elm$core$Result$Ok(one));
						default:
							var _v27 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifyWithAlias, aliases, vars, oneName, oneVars, two);
							if (_v27.$ === 1) {
								return _Utils_Tuple2(
									vars,
									$elm$core$Result$Err(
										A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
							} else {
								var unified = _v27.a;
								return unified;
							}
					}
				case 2:
					switch (two.$) {
						case 0:
							var b = two.a;
							var _v29 = A2($elm$core$Dict$get, b, vars);
							if (_v29.$ === 1) {
								return _Utils_Tuple2(
									A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars),
									$elm$core$Result$Ok(one));
							} else {
								var foundTwo = _v29.a;
								var $temp$aliases = aliases,
									$temp$vars = vars,
									$temp$one = one,
									$temp$two = foundTwo;
								aliases = $temp$aliases;
								vars = $temp$vars;
								one = $temp$one;
								two = $temp$two;
								continue unifiable;
							}
						case 2:
							return _Utils_Tuple2(
								vars,
								$elm$core$Result$Ok($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit));
						default:
							return _Utils_Tuple2(
								vars,
								$elm$core$Result$Err(
									A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
					}
				case 3:
					var valsA = one.a;
					switch (two.$) {
						case 0:
							var b = two.a;
							var _v31 = A2($elm$core$Dict$get, b, vars);
							if (_v31.$ === 1) {
								return _Utils_Tuple2(
									A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars),
									$elm$core$Result$Ok(one));
							} else {
								var foundTwo = _v31.a;
								var $temp$aliases = aliases,
									$temp$vars = vars,
									$temp$one = one,
									$temp$two = foundTwo;
								aliases = $temp$aliases;
								vars = $temp$vars;
								one = $temp$one;
								two = $temp$two;
								continue unifiable;
							}
						case 3:
							var valsB = two.a;
							var _v32 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableLists, aliases, vars, valsA, valsB, _List_Nil);
							if (!_v32.b.$) {
								var newVars = _v32.a;
								var unified = _v32.b.a;
								return _Utils_Tuple2(
									newVars,
									$elm$core$Result$Ok(
										$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(unified)));
							} else {
								var newVars = _v32.a;
								var err = _v32.b.a;
								return _Utils_Tuple2(
									newVars,
									$elm$core$Result$Err(err));
							}
						default:
							return _Utils_Tuple2(
								vars,
								$elm$core$Result$Err(
									A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
					}
				case 4:
					var fieldsA = one.a;
					switch (two.$) {
						case 0:
							var b = two.a;
							var _v34 = A2($elm$core$Dict$get, b, vars);
							if (_v34.$ === 1) {
								return _Utils_Tuple2(
									A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars),
									$elm$core$Result$Ok(one));
							} else {
								var foundTwo = _v34.a;
								var $temp$aliases = aliases,
									$temp$vars = vars,
									$temp$one = one,
									$temp$two = foundTwo;
								aliases = $temp$aliases;
								vars = $temp$vars;
								one = $temp$one;
								two = $temp$two;
								continue unifiable;
							}
						case 5:
							var _v35 = two.a;
							var twoRecName = _v35.b;
							var _v36 = two.b;
							var fieldsB = _v36.b;
							var _v37 = A2($elm$core$Dict$get, twoRecName, vars);
							if (_v37.$ === 1) {
								var _v38 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
								if (!_v38.b.$) {
									var newVars = _v38.a;
									var unifiedFields = _v38.b.a;
									return _Utils_Tuple2(
										newVars,
										$elm$core$Result$Ok(
											$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
								} else {
									var newVars = _v38.a;
									var err = _v38.b.a;
									return _Utils_Tuple2(
										newVars,
										$elm$core$Result$Err(err));
								}
							} else {
								var knownType = _v37.a;
								var _v39 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
								if (!_v39.b.$) {
									var newVars = _v39.a;
									var unifiedFields = _v39.b.a;
									return _Utils_Tuple2(
										newVars,
										$elm$core$Result$Ok(
											$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
								} else {
									var newVars = _v39.a;
									var err = _v39.b.a;
									return _Utils_Tuple2(
										newVars,
										$elm$core$Result$Err(err));
								}
							}
						case 4:
							var fieldsB = two.a;
							var _v40 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
							if (!_v40.b.$) {
								var newVars = _v40.a;
								var unifiedFields = _v40.b.a;
								return _Utils_Tuple2(
									newVars,
									$elm$core$Result$Ok(
										$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
							} else {
								var newVars = _v40.a;
								var err = _v40.b.a;
								return _Utils_Tuple2(
									newVars,
									$elm$core$Result$Err(err));
							}
						case 1:
							var twoName = two.a;
							var twoVars = two.b;
							var _v41 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifyWithAlias, aliases, vars, twoName, twoVars, one);
							if (_v41.$ === 1) {
								return _Utils_Tuple2(
									vars,
									$elm$core$Result$Err(
										A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
							} else {
								var unified = _v41.a;
								return unified;
							}
						default:
							return _Utils_Tuple2(
								vars,
								$elm$core$Result$Err(
									A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
					}
				case 5:
					var _v42 = one.a;
					var reVarName = _v42.b;
					var _v43 = one.b;
					var fieldsARange = _v43.a;
					var fieldsA = _v43.b;
					switch (two.$) {
						case 0:
							var b = two.a;
							var _v45 = A2($elm$core$Dict$get, b, vars);
							if (_v45.$ === 1) {
								return _Utils_Tuple2(
									A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars),
									$elm$core$Result$Ok(one));
							} else {
								var foundTwo = _v45.a;
								var $temp$aliases = aliases,
									$temp$vars = vars,
									$temp$one = one,
									$temp$two = foundTwo;
								aliases = $temp$aliases;
								vars = $temp$vars;
								one = $temp$one;
								two = $temp$two;
								continue unifiable;
							}
						case 5:
							var _v46 = two.a;
							var twoRecName = _v46.b;
							var _v47 = two.b;
							var fieldsB = _v47.b;
							var _v48 = A2($elm$core$Dict$get, twoRecName, vars);
							if (_v48.$ === 1) {
								var _v49 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
								if (!_v49.b.$) {
									var newVars = _v49.a;
									var unifiedFields = _v49.b.a;
									return _Utils_Tuple2(
										newVars,
										$elm$core$Result$Ok(
											$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
								} else {
									var newVars = _v49.a;
									var err = _v49.b.a;
									return _Utils_Tuple2(
										newVars,
										$elm$core$Result$Err(err));
								}
							} else {
								var knownType = _v48.a;
								var _v50 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
								if (!_v50.b.$) {
									var newVars = _v50.a;
									var unifiedFields = _v50.b.a;
									return _Utils_Tuple2(
										newVars,
										$elm$core$Result$Ok(
											$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
								} else {
									var newVars = _v50.a;
									var err = _v50.b.a;
									return _Utils_Tuple2(
										newVars,
										$elm$core$Result$Err(err));
								}
							}
						case 4:
							var fieldsB = two.a;
							var _v51 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifiableFields, aliases, vars, fieldsA, fieldsB, _List_Nil);
							if (!_v51.b.$) {
								var newVars = _v51.a;
								var unifiedFields = _v51.b.a;
								return _Utils_Tuple2(
									newVars,
									$elm$core$Result$Ok(
										$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(unifiedFields)));
							} else {
								var newVars = _v51.a;
								var err = _v51.b.a;
								return _Utils_Tuple2(
									newVars,
									$elm$core$Result$Err(err));
							}
						case 1:
							var twoName = two.a;
							var twoVars = two.b;
							var _v52 = A5($mdgriffith$elm_codegen$Internal$Compiler$unifyWithAlias, aliases, vars, twoName, twoVars, one);
							if (_v52.$ === 1) {
								return _Utils_Tuple2(
									vars,
									$elm$core$Result$Err(
										A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
							} else {
								var unified = _v52.a;
								return unified;
							}
						default:
							return _Utils_Tuple2(
								vars,
								$elm$core$Result$Err(
									A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
					}
				default:
					var oneA = one.a;
					var oneB = one.b;
					switch (two.$) {
						case 0:
							var b = two.a;
							var _v54 = A2($elm$core$Dict$get, b, vars);
							if (_v54.$ === 1) {
								return _Utils_Tuple2(
									A3($mdgriffith$elm_codegen$Internal$Compiler$addInference, b, one, vars),
									$elm$core$Result$Ok(one));
							} else {
								var foundTwo = _v54.a;
								var $temp$aliases = aliases,
									$temp$vars = vars,
									$temp$one = one,
									$temp$two = foundTwo;
								aliases = $temp$aliases;
								vars = $temp$vars;
								one = $temp$one;
								two = $temp$two;
								continue unifiable;
							}
						case 6:
							var twoA = two.a;
							var twoB = two.b;
							var _v55 = A4(
								$mdgriffith$elm_codegen$Internal$Compiler$unifiable,
								aliases,
								vars,
								$mdgriffith$elm_codegen$Internal$Compiler$denode(oneA),
								$mdgriffith$elm_codegen$Internal$Compiler$denode(twoA));
							if (!_v55.b.$) {
								var aVars = _v55.a;
								var unifiedA = _v55.b.a;
								var _v56 = A4(
									$mdgriffith$elm_codegen$Internal$Compiler$unifiable,
									aliases,
									aVars,
									$mdgriffith$elm_codegen$Internal$Compiler$denode(oneB),
									$mdgriffith$elm_codegen$Internal$Compiler$denode(twoB));
								if (!_v56.b.$) {
									var bVars = _v56.a;
									var unifiedB = _v56.b.a;
									return _Utils_Tuple2(
										bVars,
										$elm$core$Result$Ok(
											A2(
												$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
												$mdgriffith$elm_codegen$Internal$Compiler$nodify(unifiedA),
												$mdgriffith$elm_codegen$Internal$Compiler$nodify(unifiedB))));
								} else {
									var otherwise = _v56;
									return otherwise;
								}
							} else {
								var otherwise = _v55;
								return otherwise;
							}
						default:
							return _Utils_Tuple2(
								vars,
								$elm$core$Result$Err(
									A2($mdgriffith$elm_codegen$Internal$Compiler$UnableToUnify, one, two)));
					}
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$unifiableFields = F5(
	function (aliases, vars, one, two, unified) {
		unifiableFields:
		while (true) {
			var _v13 = _Utils_Tuple2(one, two);
			if (!_v13.a.b) {
				if (!_v13.b.b) {
					return _Utils_Tuple2(
						vars,
						$elm$core$Result$Ok(
							$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
								$elm$core$List$reverse(unified))));
				} else {
					return _Utils_Tuple2(
						vars,
						$elm$core$Result$Err($mdgriffith$elm_codegen$Internal$Compiler$MismatchedTypeVariables));
				}
			} else {
				var _v14 = _v13.a;
				var oneX = _v14.a;
				var oneRemain = _v14.b;
				var twoFields = _v13.b;
				var _v15 = $mdgriffith$elm_codegen$Internal$Compiler$denode(oneX);
				var oneFieldName = _v15.a;
				var oneFieldVal = _v15.b;
				var oneName = $mdgriffith$elm_codegen$Internal$Compiler$denode(oneFieldName);
				var oneVal = $mdgriffith$elm_codegen$Internal$Compiler$denode(oneFieldVal);
				var _v16 = A4($mdgriffith$elm_codegen$Internal$Compiler$getField, oneName, oneVal, twoFields, _List_Nil);
				if (!_v16.$) {
					var _v17 = _v16.a;
					var matchingFieldVal = _v17.a;
					var remainingTwo = _v17.b;
					var _v18 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, aliases, vars, oneVal, matchingFieldVal);
					var newVars = _v18.a;
					var unifiedFieldResult = _v18.b;
					if (!unifiedFieldResult.$) {
						var unifiedField = unifiedFieldResult.a;
						var $temp$aliases = aliases,
							$temp$vars = newVars,
							$temp$one = oneRemain,
							$temp$two = remainingTwo,
							$temp$unified = A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(oneName),
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(unifiedField)),
							unified);
						aliases = $temp$aliases;
						vars = $temp$vars;
						one = $temp$one;
						two = $temp$two;
						unified = $temp$unified;
						continue unifiableFields;
					} else {
						var err = unifiedFieldResult.a;
						return _Utils_Tuple2(
							newVars,
							$elm$core$Result$Err(err));
					}
				} else {
					var notFound = _v16.a;
					return _Utils_Tuple2(
						vars,
						$elm$core$Result$Err(notFound));
				}
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$unifiableLists = F5(
	function (aliases, vars, one, two, unified) {
		unifiableLists:
		while (true) {
			var _v6 = _Utils_Tuple2(one, two);
			_v6$3:
			while (true) {
				if (!_v6.a.b) {
					if (!_v6.b.b) {
						return _Utils_Tuple2(
							vars,
							$elm$core$Result$Ok(
								$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
									$elm$core$List$reverse(unified))));
					} else {
						break _v6$3;
					}
				} else {
					if (_v6.b.b) {
						if ((!_v6.a.b.b) && (!_v6.b.b.b)) {
							var _v7 = _v6.a;
							var oneX = _v7.a;
							var _v8 = _v6.b;
							var twoX = _v8.a;
							var _v9 = A4(
								$mdgriffith$elm_codegen$Internal$Compiler$unifiable,
								aliases,
								vars,
								$mdgriffith$elm_codegen$Internal$Compiler$denode(oneX),
								$mdgriffith$elm_codegen$Internal$Compiler$denode(twoX));
							if (!_v9.b.$) {
								var newVars = _v9.a;
								var un = _v9.b.a;
								return _Utils_Tuple2(
									newVars,
									$elm$core$Result$Ok(
										$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
											$elm$core$List$reverse(
												A2($elm$core$List$cons, un, unified)))));
							} else {
								var newVars = _v9.a;
								var err = _v9.b.a;
								return _Utils_Tuple2(
									newVars,
									$elm$core$Result$Err(err));
							}
						} else {
							var _v10 = _v6.a;
							var oneX = _v10.a;
							var oneRemain = _v10.b;
							var _v11 = _v6.b;
							var twoX = _v11.a;
							var twoRemain = _v11.b;
							var _v12 = A4(
								$mdgriffith$elm_codegen$Internal$Compiler$unifiable,
								aliases,
								vars,
								$mdgriffith$elm_codegen$Internal$Compiler$denode(oneX),
								$mdgriffith$elm_codegen$Internal$Compiler$denode(twoX));
							if (!_v12.b.$) {
								var newVars = _v12.a;
								var un = _v12.b.a;
								var $temp$aliases = aliases,
									$temp$vars = newVars,
									$temp$one = oneRemain,
									$temp$two = twoRemain,
									$temp$unified = A2($elm$core$List$cons, un, unified);
								aliases = $temp$aliases;
								vars = $temp$vars;
								one = $temp$one;
								two = $temp$two;
								unified = $temp$unified;
								continue unifiableLists;
							} else {
								var newVars = _v12.a;
								var err = _v12.b.a;
								return _Utils_Tuple2(
									vars,
									$elm$core$Result$Err(err));
							}
						}
					} else {
						break _v6$3;
					}
				}
			}
			return _Utils_Tuple2(
				vars,
				$elm$core$Result$Err($mdgriffith$elm_codegen$Internal$Compiler$MismatchedTypeVariables));
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$unifyWithAlias = F5(
	function (aliases, vars, typename, typeVars, typeToUnifyWith) {
		var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$getAlias, typename, aliases);
		if (_v0.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var foundAlias = _v0.a;
			var fullAliasedType = function () {
				var _v3 = foundAlias.be;
				if (!_v3.b) {
					return foundAlias.hu;
				} else {
					var makeAliasVarCache = F2(
						function (varName, _v5) {
							var varType = _v5.b;
							return _Utils_Tuple2(varName, varType);
						});
					var _v4 = A3(
						$mdgriffith$elm_codegen$Internal$Compiler$resolveVariables,
						$elm$core$Set$empty,
						$elm$core$Dict$fromList(
							A3($elm$core$List$map2, makeAliasVarCache, foundAlias.be, typeVars)),
						foundAlias.hu);
					if (!_v4.$) {
						var resolvedType = _v4.a;
						return resolvedType;
					} else {
						return foundAlias.hu;
					}
				}
			}();
			var _v1 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, aliases, vars, fullAliasedType, typeToUnifyWith);
			var returnedVars = _v1.a;
			var unifiedResult = _v1.b;
			if (!unifiedResult.$) {
				var finalInference = unifiedResult.a;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(
						returnedVars,
						$elm$core$Result$Ok(fullAliasedType)));
			} else {
				var err = unifiedResult.a;
				return $elm$core$Maybe$Nothing;
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$applyTypeHelper = F4(
	function (aliases, cache, fn, args) {
		applyTypeHelper:
		while (true) {
			switch (fn.$) {
				case 6:
					var one = fn.a;
					var two = fn.b;
					if (!args.b) {
						return $elm$core$Result$Ok(
							{iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, b: cache, h$: fn});
					} else {
						var top = args.a;
						var rest = args.b;
						var _v2 = A4(
							$mdgriffith$elm_codegen$Internal$Compiler$unifiable,
							aliases,
							cache,
							$mdgriffith$elm_codegen$Internal$Compiler$denode(one),
							top);
						if (!_v2.b.$) {
							var variableCache = _v2.a;
							var $temp$aliases = aliases,
								$temp$cache = variableCache,
								$temp$fn = $mdgriffith$elm_codegen$Internal$Compiler$denode(two),
								$temp$args = rest;
							aliases = $temp$aliases;
							cache = $temp$cache;
							fn = $temp$fn;
							args = $temp$args;
							continue applyTypeHelper;
						} else {
							var varCache = _v2.a;
							var err = _v2.b.a;
							return $elm$core$Result$Err(
								_List_fromArray(
									[err]));
						}
					}
				case 0:
					var varName = fn.a;
					if (!args.b) {
						return $elm$core$Result$Ok(
							{iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, b: cache, h$: fn});
					} else {
						var resultType = $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(varName + '_result');
						return $elm$core$Result$Ok(
							{
								iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
								b: A3(
									$mdgriffith$elm_codegen$Internal$Compiler$addInference,
									varName,
									A2($mdgriffith$elm_codegen$Internal$Compiler$makeFunction, resultType, args),
									cache),
								h$: resultType
							});
					}
				default:
					var _final = fn;
					if (!args.b) {
						return $elm$core$Result$Ok(
							{iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, b: cache, h$: fn});
					} else {
						return $elm$core$Result$Err(
							_List_fromArray(
								[
									A2($mdgriffith$elm_codegen$Internal$Compiler$FunctionAppliedToTooManyArgs, _final, args)
								]));
					}
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$mergeInferences = F2(
	function (one, two) {
		return A6(
			$elm$core$Dict$merge,
			$elm$core$Dict$insert,
			F4(
				function (key, oneVal, twoVal, d) {
					if (oneVal.$ === 5) {
						var recordName = oneVal.a;
						var _v1 = oneVal.b;
						var oneRange = _v1.a;
						var recordDefinition = _v1.b;
						if (twoVal.$ === 5) {
							var twoRecordName = twoVal.a;
							var _v3 = twoVal.b;
							var twoRange = _v3.a;
							var twoRecordDefinition = _v3.b;
							return A3(
								$elm$core$Dict$insert,
								key,
								A2(
									$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord,
									recordName,
									A2(
										$stil4m$elm_syntax$Elm$Syntax$Node$Node,
										oneRange,
										_Utils_ap(recordDefinition, twoRecordDefinition))),
								d);
						} else {
							return A3($elm$core$Dict$insert, key, oneVal, d);
						}
					} else {
						return A3($elm$core$Dict$insert, key, oneVal, d);
					}
				}),
			$elm$core$Dict$insert,
			one,
			two,
			$elm$core$Dict$empty);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$mergeArgInferences = F3(
	function (expressions, annotations, inferences) {
		mergeArgInferences:
		while (true) {
			if (!expressions.b) {
				return $elm$core$Result$Ok(
					{
						b: inferences,
						az: $elm$core$List$reverse(annotations)
					});
			} else {
				var top = expressions.a;
				var remain = expressions.b;
				var _v1 = top.bx;
				if (!_v1.$) {
					var ann = _v1.a;
					var $temp$expressions = remain,
						$temp$annotations = A2($elm$core$List$cons, ann.h$, annotations),
						$temp$inferences = A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, inferences, ann.b);
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
var $mdgriffith$elm_codegen$Internal$Compiler$applyType = F3(
	function (index, annotation, args) {
		if (annotation.$ === 1) {
			var err = annotation.a;
			return $elm$core$Result$Err(err);
		} else {
			var fnAnnotation = annotation.a;
			if ($mdgriffith$elm_codegen$Internal$Index$typecheck(index)) {
				var _v1 = A3($mdgriffith$elm_codegen$Internal$Compiler$mergeArgInferences, args, _List_Nil, fnAnnotation.b);
				if (!_v1.$) {
					var mergedArgs = _v1.a;
					return A4($mdgriffith$elm_codegen$Internal$Compiler$applyTypeHelper, fnAnnotation.iD, mergedArgs.b, fnAnnotation.h$, mergedArgs.az);
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
var $mdgriffith$elm_codegen$Internal$Index$dive = function (_v0) {
	var top = _v0.a;
	var tail = _v0.b;
	var scope = _v0.c;
	var check = _v0.d;
	return A4(
		$mdgriffith$elm_codegen$Internal$Index$Index,
		0,
		A2($elm$core$List$cons, top, tail),
		scope,
		check);
};
var $mdgriffith$elm_codegen$Internal$Compiler$expression = function (toExp) {
	return function (index) {
		return toExp(
			$mdgriffith$elm_codegen$Internal$Index$dive(index));
	};
};
var $mdgriffith$elm_codegen$Internal$Compiler$getImports = function (exp) {
	return exp.a;
};
var $mdgriffith$elm_codegen$Internal$Compiler$parens = function (expr) {
	switch (expr.$) {
		case 0:
			return expr;
		case 7:
			var i = expr.a;
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
			return $stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression(
				$mdgriffith$elm_codegen$Internal$Compiler$nodify(expr));
	}
};
var $mdgriffith$elm_codegen$Internal$Index$next = function (_v0) {
	var top = _v0.a;
	var tail = _v0.b;
	var scope = _v0.c;
	var check = _v0.d;
	return A4($mdgriffith$elm_codegen$Internal$Index$Index, top + 1, tail, scope, check);
};
var $mdgriffith$elm_codegen$Internal$Compiler$threadHelper = F3(
	function (index, exps, rendered) {
		threadHelper:
		while (true) {
			if (!exps.b) {
				return $elm$core$List$reverse(rendered);
			} else {
				var toExpDetails = exps.a;
				var remain = exps.b;
				var $temp$index = $mdgriffith$elm_codegen$Internal$Index$next(index),
					$temp$exps = remain,
					$temp$rendered = A2(
					$elm$core$List$cons,
					toExpDetails(index),
					rendered);
				index = $temp$index;
				exps = $temp$exps;
				rendered = $temp$rendered;
				continue threadHelper;
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$thread = F2(
	function (index, exps) {
		return A3($mdgriffith$elm_codegen$Internal$Compiler$threadHelper, index, exps, _List_Nil);
	});
var $mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails = F2(
	function (index, _v0) {
		var toExp = _v0;
		return _Utils_Tuple2(
			$mdgriffith$elm_codegen$Internal$Index$next(index),
			toExp(index));
	});
var $mdgriffith$elm_codegen$Elm$apply = F2(
	function (fnExp, argExpressions) {
		return $mdgriffith$elm_codegen$Internal$Compiler$expression(
			function (index) {
				var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, fnExp);
				var annotationIndex = _v0.a;
				var fnDetails = _v0.b;
				var args = A2($mdgriffith$elm_codegen$Internal$Compiler$thread, annotationIndex, argExpressions);
				return {
					bx: A3($mdgriffith$elm_codegen$Internal$Compiler$applyType, index, fnDetails.bx, args),
					jr: $stil4m$elm_syntax$Elm$Syntax$Expression$Application(
						$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
							A2(
								$elm$core$List$cons,
								fnDetails.jr,
								A2(
									$elm$core$List$map,
									A2(
										$elm$core$Basics$composeL,
										$mdgriffith$elm_codegen$Internal$Compiler$parens,
										function ($) {
											return $.jr;
										}),
									args)))),
					a: _Utils_ap(
						fnDetails.a,
						A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getImports, args))
				};
			});
	});
var $mdgriffith$elm_codegen$Elm$Annotation$function = F2(
	function (anns, _return) {
		return {
			iD: A3(
				$elm$core$List$foldl,
				F2(
					function (ann, aliases) {
						return A2(
							$mdgriffith$elm_codegen$Internal$Compiler$mergeAliases,
							$mdgriffith$elm_codegen$Elm$Annotation$getAliases(ann),
							aliases);
					}),
				$mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
				A2($elm$core$List$cons, _return, anns)),
			bx: A3(
				$elm$core$List$foldr,
				F2(
					function (ann, fn) {
						return A2(
							$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(ann),
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(fn));
					}),
				$mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(_return),
				A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation, anns)),
			a: _Utils_ap(
				$mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(_return),
				A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, anns))
		};
	});
var $mdgriffith$elm_codegen$Elm$Annotation$record = function (fields) {
	return {
		iD: A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, aliases) {
					var ann = _v0.b;
					return A2(
						$mdgriffith$elm_codegen$Internal$Compiler$mergeAliases,
						$mdgriffith$elm_codegen$Elm$Annotation$getAliases(ann),
						aliases);
				}),
			$mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
			fields),
		bx: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(
			$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
				A2(
					$elm$core$List$map,
					function (_v1) {
						var name = _v1.a;
						var ann = _v1.b;
						return _Utils_Tuple2(
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(
								$mdgriffith$elm_codegen$Internal$Format$formatValue(name)),
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(
								$mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(ann)));
					},
					fields))),
		a: A2(
			$elm$core$List$concatMap,
			A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports),
			fields)
	};
};
var $mdgriffith$elm_codegen$Elm$Annotation$tuple = F2(
	function (one, two) {
		return {
			iD: A2(
				$mdgriffith$elm_codegen$Internal$Compiler$mergeAliases,
				$mdgriffith$elm_codegen$Elm$Annotation$getAliases(one),
				$mdgriffith$elm_codegen$Elm$Annotation$getAliases(two)),
			bx: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(
				$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
					_List_fromArray(
						[
							$mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(one),
							$mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(two)
						]))),
			a: _Utils_ap(
				$mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(one),
				$mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(two))
		};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_codegen$Internal$Index$indexToString = function (_v0) {
	var top = _v0.a;
	var tail = _v0.b;
	var scope = _v0.c;
	var check = _v0.d;
	return _Utils_ap(
		(!top) ? '' : ('_' + $elm$core$String$fromInt(top)),
		function () {
			if (!tail.b) {
				return '';
			} else {
				if (!tail.b.b) {
					var one = tail.a;
					return '_' + $elm$core$String$fromInt(one);
				} else {
					if (!tail.b.b.b) {
						var one = tail.a;
						var _v2 = tail.b;
						var two = _v2.a;
						return '_' + ($elm$core$String$fromInt(one) + ('_' + $elm$core$String$fromInt(two)));
					} else {
						if (!tail.b.b.b.b) {
							var one = tail.a;
							var _v3 = tail.b;
							var two = _v3.a;
							var _v4 = _v3.b;
							var three = _v4.a;
							return '_' + ($elm$core$String$fromInt(one) + ('_' + ($elm$core$String$fromInt(two) + ('_' + $elm$core$String$fromInt(three)))));
						} else {
							return '_' + A2(
								$elm$core$String$join,
								'_',
								A2($elm$core$List$map, $elm$core$String$fromInt, tail));
						}
					}
				}
			}
		}());
};
var $mdgriffith$elm_codegen$Internal$Compiler$mapNode = F2(
	function (fn, _v0) {
		var range = _v0.a;
		var n = _v0.b;
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			range,
			fn(n));
	});
var $mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation = F2(
	function (index, ann) {
		switch (ann.$) {
			case 0:
				var str = ann.a;
				return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(
					_Utils_ap(
						str,
						$mdgriffith$elm_codegen$Internal$Index$indexToString(index)));
			case 1:
				var modName = ann.a;
				var anns = ann.b;
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
					modName,
					A2(
						$elm$core$List$map,
						$mdgriffith$elm_codegen$Internal$Compiler$mapNode(
							$mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation(index)),
						anns));
			case 2:
				return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit;
			case 3:
				var tupled = ann.a;
				return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(
					A2(
						$elm$core$List$map,
						$mdgriffith$elm_codegen$Internal$Compiler$mapNode(
							$mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation(index)),
						tupled));
			case 4:
				var recordDefinition = ann.a;
				return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(
					A2(
						$elm$core$List$map,
						$mdgriffith$elm_codegen$Internal$Compiler$protectField(index),
						recordDefinition));
			case 5:
				var recordName = ann.a;
				var _v3 = ann.b;
				var recordRange = _v3.a;
				var recordDefinition = _v3.b;
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord,
					A2(
						$mdgriffith$elm_codegen$Internal$Compiler$mapNode,
						function (n) {
							return _Utils_ap(
								n,
								$mdgriffith$elm_codegen$Internal$Index$indexToString(index));
						},
						recordName),
					A2(
						$stil4m$elm_syntax$Elm$Syntax$Node$Node,
						recordRange,
						A2(
							$elm$core$List$map,
							$mdgriffith$elm_codegen$Internal$Compiler$protectField(index),
							recordDefinition)));
			default:
				var one = ann.a;
				var two = ann.b;
				return A2(
					$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
					A2(
						$mdgriffith$elm_codegen$Internal$Compiler$mapNode,
						$mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation(index),
						one),
					A2(
						$mdgriffith$elm_codegen$Internal$Compiler$mapNode,
						$mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation(index),
						two));
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$protectField = F2(
	function (index, _v0) {
		var nodeRange = _v0.a;
		var _v1 = _v0.b;
		var nodedName = _v1.a;
		var nodedType = _v1.b;
		return A2(
			$stil4m$elm_syntax$Elm$Syntax$Node$Node,
			nodeRange,
			_Utils_Tuple2(
				nodedName,
				A2(
					$mdgriffith$elm_codegen$Internal$Compiler$mapNode,
					$mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation(index),
					nodedType)));
	});
var $mdgriffith$elm_codegen$Internal$Compiler$getInnerInference = F2(
	function (index, _v0) {
		var details = _v0;
		return {
			iD: details.iD,
			b: $elm$core$Dict$empty,
			h$: A2($mdgriffith$elm_codegen$Internal$Compiler$protectAnnotation, index, details.bx)
		};
	});
var $mdgriffith$elm_codegen$Internal$Index$protectTypeName = F2(
	function (base, index) {
		var top = index.a;
		var tail = index.b;
		var scope = index.c;
		var check = index.d;
		if (!tail.b) {
			return $mdgriffith$elm_codegen$Internal$Format$formatValue(base);
		} else {
			return $mdgriffith$elm_codegen$Internal$Format$formatValue(
				_Utils_ap(
					base,
					$mdgriffith$elm_codegen$Internal$Index$indexToString(index)));
		}
	});
var $mdgriffith$elm_codegen$Elm$value = function (details) {
	return function (index) {
		return {
			bx: function () {
				var _v0 = details.bx;
				if (_v0.$ === 1) {
					var typename = A2($mdgriffith$elm_codegen$Internal$Index$protectTypeName, details.ao, index);
					return $elm$core$Result$Ok(
						{
							iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
							b: $elm$core$Dict$empty,
							h$: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(typename)
						});
				} else {
					var ann = _v0.a;
					return $elm$core$Result$Ok(
						A2($mdgriffith$elm_codegen$Internal$Compiler$getInnerInference, index, ann));
				}
			}(),
			jr: A2(
				$stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue,
				details.ei,
				$mdgriffith$elm_codegen$Internal$Format$sanitize(details.ao)),
			a: function () {
				var _v1 = details.bx;
				if (_v1.$ === 1) {
					var _v2 = details.ei;
					if (!_v2.b) {
						return _List_Nil;
					} else {
						return _List_fromArray(
							[details.ei]);
					}
				} else {
					var ann = _v1.a;
					var _v3 = details.ei;
					if (!_v3.b) {
						return $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(ann);
					} else {
						return A2(
							$elm$core$List$cons,
							details.ei,
							$mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(ann));
					}
				}
			}()
		};
	};
};
var $author$project$Gen$App$call_ = {
	cW: function (elementArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$record(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'init',
											A2(
												$mdgriffith$elm_codegen$Elm$Annotation$function,
												_List_fromArray(
													[
														$mdgriffith$elm_codegen$Elm$Annotation$var('flags')
													]),
												A2(
													$mdgriffith$elm_codegen$Elm$Annotation$tuple,
													$mdgriffith$elm_codegen$Elm$Annotation$var('model'),
													A3(
														$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
														_List_Nil,
														'Cmd',
														_List_fromArray(
															[
																$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
															]))))),
											_Utils_Tuple2(
											'view',
											A2(
												$mdgriffith$elm_codegen$Elm$Annotation$function,
												_List_fromArray(
													[
														$mdgriffith$elm_codegen$Elm$Annotation$var('model')
													]),
												A3(
													$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
													_List_Nil,
													'Html',
													_List_fromArray(
														[
															$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
														])))),
											_Utils_Tuple2(
											'update',
											A2(
												$mdgriffith$elm_codegen$Elm$Annotation$function,
												_List_fromArray(
													[
														$mdgriffith$elm_codegen$Elm$Annotation$var('msg'),
														$mdgriffith$elm_codegen$Elm$Annotation$var('model')
													]),
												A2(
													$mdgriffith$elm_codegen$Elm$Annotation$tuple,
													$mdgriffith$elm_codegen$Elm$Annotation$var('model'),
													A3(
														$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
														_List_Nil,
														'Cmd',
														_List_fromArray(
															[
																$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
															]))))),
											_Utils_Tuple2(
											'subscriptions',
											A2(
												$mdgriffith$elm_codegen$Elm$Annotation$function,
												_List_fromArray(
													[
														$mdgriffith$elm_codegen$Elm$Annotation$var('model')
													]),
												A3(
													$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
													_List_Nil,
													'Sub',
													_List_fromArray(
														[
															$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
														]))))
										]))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_Nil,
								'Prog',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('flags'),
										$mdgriffith$elm_codegen$Elm$Annotation$var('model'),
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									])))),
					ei: _List_fromArray(
						['App']),
					ao: 'element'
				}),
			_List_fromArray(
				[elementArg]));
	}
};
var $mdgriffith$elm_codegen$Elm$Annotation$typed = F3(
	function (mod, name, args) {
		return {
			iD: A3(
				$elm$core$List$foldl,
				F2(
					function (ann, aliases) {
						return A2(
							$mdgriffith$elm_codegen$Internal$Compiler$mergeAliases,
							$mdgriffith$elm_codegen$Elm$Annotation$getAliases(ann),
							aliases);
					}),
				$mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
				args),
			bx: A2(
				$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
				$mdgriffith$elm_codegen$Internal$Compiler$nodify(
					_Utils_Tuple2(mod, name)),
				$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
					A2($elm$core$List$map, $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation, args))),
			a: A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, args)
		};
	});
var $mdgriffith$elm_codegen$Elm$Annotation$cmd = function (inner) {
	return A3(
		$mdgriffith$elm_codegen$Elm$Annotation$typed,
		_List_Nil,
		'Cmd',
		_List_fromArray(
			[inner]));
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$LambdaExpression = function (a) {
	return {$: 17, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern = function (a) {
	return {$: 11, a: a};
};
var $mdgriffith$elm_codegen$Internal$Index$getName = F2(
	function (desiredName, index) {
		var top = index.a;
		var tail = index.b;
		var scope = index.c;
		var check = index.d;
		var formattedName = $mdgriffith$elm_codegen$Internal$Format$formatValue(desiredName);
		if (!A2($elm$core$Set$member, formattedName, scope)) {
			return _Utils_Tuple2(
				formattedName,
				A4(
					$mdgriffith$elm_codegen$Internal$Index$Index,
					top,
					tail,
					A2($elm$core$Set$insert, formattedName, scope),
					check));
		} else {
			var protectedName = _Utils_ap(
				formattedName,
				$elm$core$String$fromInt(top));
			if (!A2($elm$core$Set$member, protectedName, scope)) {
				return _Utils_Tuple2(
					protectedName,
					A4(
						$mdgriffith$elm_codegen$Internal$Index$Index,
						top + 1,
						tail,
						A2($elm$core$Set$insert, protectedName, scope),
						check));
			} else {
				var protectedNameLevel2 = _Utils_ap(
					formattedName,
					$mdgriffith$elm_codegen$Internal$Index$indexToString(index));
				return _Utils_Tuple2(
					protectedNameLevel2,
					A4(
						$mdgriffith$elm_codegen$Internal$Index$Index,
						top + 1,
						tail,
						A2($elm$core$Set$insert, protectedNameLevel2, scope),
						check));
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$toVarMaybeType = F3(
	function (index, desiredName, maybeAnnotation) {
		var _v0 = A2($mdgriffith$elm_codegen$Internal$Index$getName, desiredName, index);
		var name = _v0.a;
		var newIndex = _v0.b;
		var _v1 = function () {
			if (maybeAnnotation.$ === 1) {
				return {
					iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
					bx: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(
						A2($mdgriffith$elm_codegen$Internal$Index$protectTypeName, desiredName, index)),
					a: _List_Nil
				};
			} else {
				var ann = maybeAnnotation.a;
				return ann;
			}
		}();
		var imports = _v1.a;
		var annotation = _v1.bx;
		var aliases = _v1.iD;
		return {
			jK: newIndex,
			ao: name,
			h$: annotation,
			g: function (ignoredIndex_) {
				return {
					bx: $elm$core$Result$Ok(
						{iD: aliases, b: $elm$core$Dict$empty, h$: annotation}),
					jr: A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, name),
					a: imports
				};
			}
		};
	});
var $mdgriffith$elm_codegen$Elm$fn = F2(
	function (_v0, toExpression) {
		var oneBaseName = _v0.a;
		var maybeAnnotation = _v0.b;
		return $mdgriffith$elm_codegen$Internal$Compiler$expression(
			function (index) {
				var one = A3($mdgriffith$elm_codegen$Internal$Compiler$toVarMaybeType, index, oneBaseName, maybeAnnotation);
				var _v1 = toExpression(one.g);
				var toExpr = _v1;
				var _return = toExpr(one.jK);
				return {
					bx: function () {
						var _v2 = _return.bx;
						if (_v2.$ === 1) {
							var err = _v2.a;
							return _return.bx;
						} else {
							var returnAnnotation = _v2.a;
							return $elm$core$Result$Ok(
								{
									iD: returnAnnotation.iD,
									b: returnAnnotation.b,
									h$: A2(
										$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
										$mdgriffith$elm_codegen$Internal$Compiler$nodify(one.h$),
										$mdgriffith$elm_codegen$Internal$Compiler$nodify(returnAnnotation.h$))
								});
						}
					}(),
					jr: $stil4m$elm_syntax$Elm$Syntax$Expression$LambdaExpression(
						{
							B: _List_fromArray(
								[
									$mdgriffith$elm_codegen$Internal$Compiler$nodify(
									$stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(one.ao))
								]),
							jr: $mdgriffith$elm_codegen$Internal$Compiler$nodify(_return.jr)
						}),
					a: _return.a
				};
			});
	});
var $author$project$Gen$Platform$Sub$none = $mdgriffith$elm_codegen$Elm$value(
	{
		bx: $elm$core$Maybe$Just(
			A3(
				$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
				_List_Nil,
				'Sub',
				_List_fromArray(
					[
						$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
					]))),
		ei: _List_fromArray(
			['Platform', 'Sub']),
		ao: 'none'
	});
var $mdgriffith$elm_codegen$Internal$Compiler$DuplicateFieldInRecord = function (a) {
	return {$: 5, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr = function (a) {
	return {$: 18, a: a};
};
var $mdgriffith$elm_codegen$Elm$record = function (fields) {
	return $mdgriffith$elm_codegen$Internal$Compiler$expression(
		function (index) {
			var unified = A3(
				$elm$core$List$foldl,
				F2(
					function (_v4, found) {
						var unformattedFieldName = _v4.a;
						var fieldExpression = _v4.b;
						var fieldName = $mdgriffith$elm_codegen$Internal$Format$formatValue(unformattedFieldName);
						var _v5 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, found.jK, fieldExpression);
						var newIndex = _v5.a;
						var exp = _v5.b;
						return {
							I: function () {
								if (A2($elm$core$Set$member, fieldName, found.aL)) {
									return A2(
										$elm$core$List$cons,
										$mdgriffith$elm_codegen$Internal$Compiler$DuplicateFieldInRecord(fieldName),
										found.I);
								} else {
									var _v6 = exp.bx;
									if (_v6.$ === 1) {
										if (!_v6.a.b) {
											return found.I;
										} else {
											var errs = _v6.a;
											return _Utils_ap(errs, found.I);
										}
									} else {
										var ann = _v6.a;
										return found.I;
									}
								}
							}(),
							J: function () {
								var _v7 = exp.bx;
								if (_v7.$ === 1) {
									var err = _v7.a;
									return found.J;
								} else {
									var ann = _v7.a;
									return A2(
										$elm$core$List$cons,
										_Utils_Tuple2(
											$mdgriffith$elm_codegen$Internal$Format$formatValue(fieldName),
											ann),
										found.J);
								}
							}(),
							aX: A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									$mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName),
									$mdgriffith$elm_codegen$Internal$Compiler$nodify(exp.jr)),
								found.aX),
							a: _Utils_ap(exp.a, found.a),
							jK: newIndex,
							aL: A2($elm$core$Set$insert, fieldName, found.aL)
						};
					}),
				{I: _List_Nil, J: _List_Nil, aX: _List_Nil, a: _List_Nil, jK: index, aL: $elm$core$Set$empty},
				fields);
			return {
				bx: function () {
					var _v0 = unified.I;
					if (!_v0.b) {
						return $elm$core$Result$Ok(
							{
								iD: A3(
									$elm$core$List$foldl,
									F2(
										function (_v1, gathered) {
											var name = _v1.a;
											var ann = _v1.b;
											return A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, ann.iD, gathered);
										}),
									$mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
									unified.J),
								b: A3(
									$elm$core$List$foldl,
									F2(
										function (_v2, gathered) {
											var name = _v2.a;
											var ann = _v2.b;
											return A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, ann.b, gathered);
										}),
									$elm$core$Dict$empty,
									unified.J),
								h$: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Record(
									$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
										A2(
											$elm$core$List$map,
											function (_v3) {
												var name = _v3.a;
												var ann = _v3.b;
												return _Utils_Tuple2(
													$mdgriffith$elm_codegen$Internal$Compiler$nodify(name),
													$mdgriffith$elm_codegen$Internal$Compiler$nodify(ann.h$));
											},
											$elm$core$List$reverse(unified.J))))
							});
					} else {
						var errs = _v0;
						return $elm$core$Result$Err(errs);
					}
				}(),
				jr: $stil4m$elm_syntax$Elm$Syntax$Expression$RecordExpr(
					$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
						$elm$core$List$reverse(unified.aX))),
				a: unified.a
			};
		});
};
var $mdgriffith$elm_codegen$Elm$Annotation$unit = {iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, bx: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit, a: _List_Nil};
var $mdgriffith$elm_codegen$Internal$Compiler$unifyOn = F2(
	function (_v0, res) {
		var annDetails = _v0;
		if (res.$ === 1) {
			return res;
		} else {
			var inf = res.a;
			var _v2 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, inf.iD, inf.b, annDetails.bx, inf.h$);
			var newInferences = _v2.a;
			var finalResult = _v2.b;
			if (!finalResult.$) {
				var finalType = finalResult.a;
				return $elm$core$Result$Ok(
					{
						iD: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, annDetails.iD, inf.iD),
						b: newInferences,
						h$: finalType
					});
			} else {
				var err = finalResult.a;
				return $elm$core$Result$Err(
					_List_fromArray(
						[err]));
			}
		}
	});
var $mdgriffith$elm_codegen$Elm$withType = F2(
	function (ann, _v0) {
		var annDetails = ann;
		var toExp = _v0;
		return function (index) {
			var exp = toExp(index);
			return _Utils_update(
				exp,
				{
					bx: function () {
						var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$unifyOn, ann, exp.bx);
						if (!_v1.$) {
							var unified = _v1.a;
							return $elm$core$Result$Ok(unified);
						} else {
							var _v2 = exp.bx;
							if (!_v2.$) {
								var expressionAnnotation = _v2.a;
								return $elm$core$Result$Ok(
									{iD: expressionAnnotation.iD, b: expressionAnnotation.b, h$: annDetails.bx});
							} else {
								var err = _v2.a;
								return $elm$core$Result$Ok(
									{iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, b: $elm$core$Dict$empty, h$: annDetails.bx});
							}
						}
					}(),
					a: _Utils_ap(exp.a, annDetails.a)
				});
		};
	});
var $author$project$Interactive$callMain = A2(
	$mdgriffith$elm_codegen$Elm$withType,
	A3(
		$author$project$Gen$App$annotation_.km,
		$mdgriffith$elm_codegen$Elm$Annotation$unit,
		A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, 'Model'),
		A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, 'Msg')),
	$author$project$Gen$App$call_.cW(
		$mdgriffith$elm_codegen$Elm$record(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'init',
					$mdgriffith$elm_codegen$Elm$value(
						{
							bx: $elm$core$Maybe$Just(
								A2(
									$mdgriffith$elm_codegen$Elm$Annotation$function,
									_List_fromArray(
										[$mdgriffith$elm_codegen$Elm$Annotation$unit]),
									A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$author$project$Interactive$appTypes._,
										$mdgriffith$elm_codegen$Elm$Annotation$cmd(
											$mdgriffith$elm_codegen$Elm$Annotation$var('msg'))))),
							ei: _List_Nil,
							ao: 'init'
						})),
					_Utils_Tuple2(
					'update',
					$mdgriffith$elm_codegen$Elm$value(
						{
							bx: $elm$core$Maybe$Just(
								A2(
									$mdgriffith$elm_codegen$Elm$Annotation$function,
									_List_fromArray(
										[$author$project$Interactive$appTypes.x, $author$project$Interactive$appTypes._]),
									A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$author$project$Interactive$appTypes._,
										$mdgriffith$elm_codegen$Elm$Annotation$cmd(
											$mdgriffith$elm_codegen$Elm$Annotation$var('msg'))))),
							ei: _List_Nil,
							ao: 'update'
						})),
					_Utils_Tuple2(
					'view',
					$mdgriffith$elm_codegen$Elm$value(
						{bx: $elm$core$Maybe$Nothing, ei: _List_Nil, ao: 'view'})),
					_Utils_Tuple2(
					'subscriptions',
					A2(
						$mdgriffith$elm_codegen$Elm$fn,
						_Utils_Tuple2('model', $elm$core$Maybe$Nothing),
						function (_v0) {
							return $author$project$Gen$Platform$Sub$none;
						}))
				]))));
var $author$project$Gen$Element$alignRight = $mdgriffith$elm_codegen$Elm$value(
	{
		bx: $elm$core$Maybe$Just(
			A3(
				$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
				_List_fromArray(
					['Element']),
				'Attribute',
				_List_fromArray(
					[
						$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
					]))),
		ei: _List_fromArray(
			['Element']),
		ao: 'alignRight'
	});
var $mdgriffith$elm_codegen$Elm$Case$Branch = $elm$core$Basics$identity;
var $stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern = F2(
	function (a, b) {
		return {$: 12, a: a, b: b};
	});
var $mdgriffith$elm_codegen$Elm$Case$branch0 = F2(
	function (name, exp) {
		return function (index) {
			return _Utils_Tuple3(
				index,
				A2(
					$stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern,
					{
						a1: _List_Nil,
						ao: $mdgriffith$elm_codegen$Internal$Format$formatType(name)
					},
					_List_Nil),
				exp);
		};
	});
var $mdgriffith$elm_codegen$Internal$Compiler$toVarWithType = F3(
	function (index, desiredName, _v0) {
		var ann = _v0;
		var _v1 = A2($mdgriffith$elm_codegen$Internal$Index$getName, desiredName, index);
		var name = _v1.a;
		var newIndex = _v1.b;
		return {
			f: function (ignoredIndex_) {
				return {
					bx: $elm$core$Result$Ok(
						{iD: ann.iD, b: $elm$core$Dict$empty, h$: ann.bx}),
					jr: A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, name),
					a: ann.a
				};
			},
			jK: newIndex,
			ao: name
		};
	});
var $mdgriffith$elm_codegen$Elm$Case$branch1 = F3(
	function (name, _v0, toExp) {
		var argName = _v0.a;
		var argType = _v0.b;
		return function (index) {
			var _var = A3($mdgriffith$elm_codegen$Internal$Compiler$toVarWithType, index, argName, argType);
			return _Utils_Tuple3(
				_var.jK,
				A2(
					$stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern,
					{
						a1: _List_Nil,
						ao: $mdgriffith$elm_codegen$Internal$Format$formatType(name)
					},
					_List_fromArray(
						[
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(
							$stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(_var.ao))
						])),
				toExp(_var.f));
		};
	});
var $author$project$Gen$Element$Border$color = function (colorArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Color',
								_List_Nil)
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attr',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('decorative'),
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element', 'Border']),
				ao: 'color'
			}),
		_List_fromArray(
			[colorArg]));
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$CaseExpression = function (a) {
	return {$: 16, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$EmptyCaseStatement = {$: 2};
var $mdgriffith$elm_codegen$Elm$Case$combineInferences = F2(
	function (infs, infResult) {
		if (!infResult.$) {
			var inferred = infResult.a;
			return $elm$core$Result$Ok(
				_Utils_update(
					inferred,
					{
						b: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, infs, inferred.b)
					}));
		} else {
			var err = infResult.a;
			return $elm$core$Result$Err(err);
		}
	});
var $mdgriffith$elm_codegen$Elm$Case$captureCaseHelper = F3(
	function (mainCaseExpressionModule, _v0, accum) {
		var toBranch = _v0;
		var _v1 = toBranch(
			$mdgriffith$elm_codegen$Internal$Index$dive(accum.jK));
		var branchIndex = _v1.a;
		var originalPattern = _v1.b;
		var caseExpression = _v1.c;
		var _v2 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, branchIndex, caseExpression);
		var newIndex = _v2.a;
		var exp = _v2.b;
		var pattern = function () {
			if (!mainCaseExpressionModule.b) {
				return originalPattern;
			} else {
				if (originalPattern.$ === 12) {
					var named = originalPattern.a;
					var vars = originalPattern.b;
					return A2(
						$stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern,
						{a1: mainCaseExpressionModule, ao: named.ao},
						vars);
				} else {
					return originalPattern;
				}
			}
		}();
		return {
			bx: function () {
				var _v3 = accum.bx;
				if (_v3.$ === 1) {
					return $elm$core$Maybe$Just(exp.bx);
				} else {
					if (!_v3.a.$) {
						var gatheredAnnotation = _v3.a.a;
						return $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Internal$Compiler$unifyOn,
								{iD: gatheredAnnotation.iD, bx: gatheredAnnotation.h$, a: _List_Nil},
								A2($mdgriffith$elm_codegen$Elm$Case$combineInferences, gatheredAnnotation.b, exp.bx)));
					} else {
						var err = _v3.a;
						return $elm$core$Maybe$Just(err);
					}
				}
			}(),
			j: A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(pattern),
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(exp.jr)),
				accum.j),
			a: _Utils_ap(accum.a, exp.a),
			jK: accum.jK
		};
	});
var $mdgriffith$elm_codegen$Internal$Compiler$importInferences = F2(
	function (one, two) {
		return {
			iD: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, one.iD, two.iD),
			b: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, one.b, two.b),
			h$: two.h$
		};
	});
var $mdgriffith$elm_codegen$Elm$Case$captureCase = F4(
	function (mainExpression, mainExpressionTypeModule, index, branches) {
		var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, mainExpression);
		var branchIndex = _v0.a;
		var mainExpressionDetails = _v0.b;
		var caseExp = A3(
			$elm$core$List$foldl,
			$mdgriffith$elm_codegen$Elm$Case$captureCaseHelper(mainExpressionTypeModule),
			{bx: $elm$core$Maybe$Nothing, j: _List_Nil, a: _List_Nil, jK: branchIndex},
			branches);
		return _Utils_Tuple2(
			mainExpressionDetails,
			_Utils_update(
				caseExp,
				{
					bx: function () {
						var _v1 = caseExp.bx;
						if ((!_v1.$) && (!_v1.a.$)) {
							var inference = _v1.a.a;
							var _v2 = mainExpressionDetails.bx;
							if (_v2.$ === 1) {
								var err = _v2.a;
								return $elm$core$Maybe$Just(
									$elm$core$Result$Err(err));
							} else {
								var mainAnn = _v2.a;
								return $elm$core$Maybe$Just(
									$elm$core$Result$Ok(
										A2($mdgriffith$elm_codegen$Internal$Compiler$importInferences, mainAnn, inference)));
							}
						} else {
							return caseExp.bx;
						}
					}()
				}));
	});
var $mdgriffith$elm_codegen$Internal$Compiler$getTypeModule = function (_v0) {
	var annotation = _v0;
	var _v1 = annotation.bx;
	if (_v1.$ === 1) {
		var _v2 = _v1.a;
		var _v3 = _v2.b;
		var mod = _v3.a;
		var typeName = _v3.b;
		return mod;
	} else {
		return _List_Nil;
	}
};
var $mdgriffith$elm_codegen$Elm$Case$custom = F3(
	function (mainExpression, annotation, branches) {
		return function (index) {
			var myMain = A2($mdgriffith$elm_codegen$Elm$withType, annotation, mainExpression);
			var _v0 = A4(
				$mdgriffith$elm_codegen$Elm$Case$captureCase,
				myMain,
				$mdgriffith$elm_codegen$Internal$Compiler$getTypeModule(annotation),
				$mdgriffith$elm_codegen$Internal$Index$dive(index),
				branches);
			var expr = _v0.a;
			var gathered = _v0.b;
			return {
				bx: function () {
					var _v1 = gathered.bx;
					if (_v1.$ === 1) {
						return $elm$core$Result$Err(
							_List_fromArray(
								[$mdgriffith$elm_codegen$Internal$Compiler$EmptyCaseStatement]));
					} else {
						var ann = _v1.a;
						return ann;
					}
				}(),
				jr: $stil4m$elm_syntax$Elm$Syntax$Expression$CaseExpression(
					{
						j: $elm$core$List$reverse(gathered.j),
						jr: $mdgriffith$elm_codegen$Internal$Compiler$nodify(expr.jr)
					}),
				a: _Utils_ap(expr.a, gathered.a)
			};
		};
	});
var $mdgriffith$elm_codegen$Elm$customType = F2(
	function (name, variants) {
		return $mdgriffith$elm_codegen$Internal$Compiler$Declaration(
			{
				aT: $elm$core$Maybe$Nothing,
				dh: $mdgriffith$elm_codegen$Internal$Compiler$NotExposed,
				a: A2(
					$elm$core$List$concatMap,
					function (_v0) {
						var listAnn = _v0.b;
						return A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports, listAnn);
					},
					variants),
				ao: name,
				ac: function (index) {
					return {
						T: _List_Nil,
						jf: $stil4m$elm_syntax$Elm$Syntax$Declaration$CustomTypeDeclaration(
							{
								ja: A2(
									$elm$core$List$map,
									function (_v1) {
										var varName = _v1.a;
										var vars = _v1.b;
										return $mdgriffith$elm_codegen$Internal$Compiler$nodify(
											{
												aQ: A2(
													$elm$core$List$map,
													A2($elm$core$Basics$composeR, $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation, $mdgriffith$elm_codegen$Internal$Compiler$nodify),
													vars),
												ao: $mdgriffith$elm_codegen$Internal$Compiler$nodify(
													$mdgriffith$elm_codegen$Internal$Format$formatType(varName))
											});
									},
									variants),
								aU: $elm$core$Maybe$Nothing,
								dP: A2(
									$elm$core$List$concatMap,
									function (_v2) {
										var listAnn = _v2.b;
										return A2(
											$elm$core$List$concatMap,
											A2(
												$elm$core$Basics$composeL,
												$elm$core$List$map($mdgriffith$elm_codegen$Internal$Compiler$nodify),
												$mdgriffith$elm_codegen$Internal$Compiler$getGenerics),
											listAnn);
									},
									variants),
								ao: $mdgriffith$elm_codegen$Internal$Compiler$nodify(
									$mdgriffith$elm_codegen$Internal$Format$formatType(name))
							}),
						k0: $elm$core$Maybe$Nothing
					};
				}
			});
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$ListExpr = function (a) {
	return {$: 19, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$MismatchedList = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_codegen$Internal$Compiler$unifyHelper = F2(
	function (exps, existing) {
		unifyHelper:
		while (true) {
			if (!exps.b) {
				return $elm$core$Result$Ok(existing);
			} else {
				var top = exps.a;
				var remain = exps.b;
				var _v1 = top.bx;
				if (!_v1.$) {
					var ann = _v1.a;
					var _v2 = A4($mdgriffith$elm_codegen$Internal$Compiler$unifiable, ann.iD, ann.b, ann.h$, existing.h$);
					if (_v2.b.$ === 1) {
						var err = _v2.b.a;
						return $elm$core$Result$Err(
							_List_fromArray(
								[
									A2($mdgriffith$elm_codegen$Internal$Compiler$MismatchedList, ann.h$, existing.h$)
								]));
					} else {
						var cache = _v2.a;
						var _new = _v2.b.a;
						var $temp$exps = remain,
							$temp$existing = {
							iD: existing.iD,
							b: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, existing.b, cache),
							h$: _new
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
var $mdgriffith$elm_codegen$Internal$Compiler$unify = function (exps) {
	if (!exps.b) {
		return $elm$core$Result$Ok(
			{
				iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
				b: $elm$core$Dict$empty,
				h$: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType('a')
			});
	} else {
		var top = exps.a;
		var remain = exps.b;
		var _v1 = top.bx;
		if (!_v1.$) {
			var ann = _v1.a;
			return A2($mdgriffith$elm_codegen$Internal$Compiler$unifyHelper, remain, ann);
		} else {
			var err = _v1.a;
			return $elm$core$Result$Err(err);
		}
	}
};
var $mdgriffith$elm_codegen$Elm$list = function (exprs) {
	return $mdgriffith$elm_codegen$Internal$Compiler$expression(
		function (index) {
			var exprDetails = A2($mdgriffith$elm_codegen$Internal$Compiler$thread, index, exprs);
			return {
				bx: A2(
					$elm$core$Result$map,
					function (inner) {
						return {
							iD: inner.iD,
							b: inner.b,
							h$: A2(
								$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(
									_Utils_Tuple2(_List_Nil, 'List')),
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Internal$Compiler$nodify(inner.h$)
									]))
						};
					},
					$mdgriffith$elm_codegen$Internal$Compiler$unify(exprDetails)),
				jr: $stil4m$elm_syntax$Elm$Syntax$Expression$ListExpr(
					A2(
						$elm$core$List$map,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.jr;
							},
							$mdgriffith$elm_codegen$Internal$Compiler$nodify),
						exprDetails)),
				a: A2($elm$core$List$concatMap, $mdgriffith$elm_codegen$Internal$Compiler$getImports, exprDetails)
			};
		});
};
var $mdgriffith$elm_codegen$Elm$Annotation$list = function (inner) {
	return A3(
		$mdgriffith$elm_codegen$Elm$Annotation$typed,
		_List_Nil,
		'List',
		_List_fromArray(
			[inner]));
};
var $author$project$Gen$Element$el = F2(
	function (elArg, elArg0) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Element']),
										'Attribute',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
											]))),
									A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Element']),
									'Element',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
										]))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Element',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									])))),
					ei: _List_fromArray(
						['Element']),
					ao: 'el'
				}),
			_List_fromArray(
				[
					$mdgriffith$elm_codegen$Elm$list(elArg),
					elArg0
				]));
	});
var $mdgriffith$elm_codegen$Elm$Op$BinOp = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $stil4m$elm_syntax$Elm$Syntax$Infix$Left = 0;
var $stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication = F4(
	function (a, b, c, d) {
		return {$: 2, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_codegen$Elm$Op$applyInfix = F4(
	function (_v0, infixAnnotation, l, r) {
		var symbol = _v0.a;
		var dir = _v0.b;
		return function (index) {
			var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, l);
			var leftIndex = _v1.a;
			var left = _v1.b;
			var _v2 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, leftIndex, r);
			var rightIndex = _v2.a;
			var right = _v2.b;
			var annotationIndex = $mdgriffith$elm_codegen$Internal$Index$next(rightIndex);
			return {
				bx: A3(
					$mdgriffith$elm_codegen$Internal$Compiler$applyType,
					index,
					$elm$core$Result$Ok(
						{iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, b: $elm$core$Dict$empty, h$: infixAnnotation}),
					_List_fromArray(
						[left, right])),
				jr: A4(
					$stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication,
					symbol,
					dir,
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(
						$mdgriffith$elm_codegen$Internal$Compiler$parens(left.jr)),
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(
						$mdgriffith$elm_codegen$Internal$Compiler$parens(right.jr))),
				a: _Utils_ap(left.a, right.a)
			};
		};
	});
var $mdgriffith$elm_codegen$Internal$Types$nodify = function (exp) {
	return A2($stil4m$elm_syntax$Elm$Syntax$Node$Node, $stil4m$elm_syntax$Elm$Syntax$Range$emptyRange, exp);
};
var $mdgriffith$elm_codegen$Internal$Types$bool = A2(
	$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
	$mdgriffith$elm_codegen$Internal$Types$nodify(
		_Utils_Tuple2(_List_Nil, 'Bool')),
	_List_Nil);
var $mdgriffith$elm_codegen$Internal$Types$function = F2(
	function (args, _return) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (ann, fn) {
					return A2(
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
						$mdgriffith$elm_codegen$Internal$Types$nodify(ann),
						$mdgriffith$elm_codegen$Internal$Types$nodify(fn));
				}),
			_return,
			args);
	});
var $mdgriffith$elm_codegen$Internal$Types$formatValue = function (str) {
	var formatted = _Utils_eq(
		$elm$core$String$toUpper(str),
		str) ? $elm$core$String$toLower(str) : _Utils_ap(
		$elm$core$String$toLower(
			A2($elm$core$String$left, 1, str)),
		A2($elm$core$String$dropLeft, 1, str));
	return $mdgriffith$elm_codegen$Internal$Format$sanitize(formatted);
};
var $mdgriffith$elm_codegen$Internal$Types$var = function (name) {
	return $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(
		$mdgriffith$elm_codegen$Internal$Types$formatValue(name));
};
var $mdgriffith$elm_codegen$Elm$Op$equal = A2(
	$mdgriffith$elm_codegen$Elm$Op$applyInfix,
	A3($mdgriffith$elm_codegen$Elm$Op$BinOp, '==', 0, 4),
	A2(
		$mdgriffith$elm_codegen$Internal$Types$function,
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Internal$Types$var('a'),
				$mdgriffith$elm_codegen$Internal$Types$var('a')
			]),
		$mdgriffith$elm_codegen$Internal$Types$bool));
var $mdgriffith$elm_codegen$Elm$Declare$fn = F3(
	function (name, one, toExp) {
		var funcExp = A2($mdgriffith$elm_codegen$Elm$fn, one, toExp);
		var call = F2(
			function (importFrom, argOne) {
				return A2(
					$mdgriffith$elm_codegen$Elm$apply,
					function (index) {
						var toFnExp = funcExp;
						var fnExp = toFnExp(index);
						return {
							bx: fnExp.bx,
							jr: A2(
								$stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue,
								importFrom,
								$mdgriffith$elm_codegen$Internal$Format$sanitize(name)),
							a: fnExp.a
						};
					},
					_List_fromArray(
						[argOne]));
			});
		return {
			G: call(_List_Nil),
			H: call,
			jf: A2($mdgriffith$elm_codegen$Elm$declaration, name, funcExp)
		};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccess = F2(
	function (a, b) {
		return {$: 20, a: a, b: b};
	});
var $mdgriffith$elm_codegen$Internal$Compiler$AttemptingGetOnTypeNameNotAnAlias = function (a) {
	return {$: 9, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$AttemptingToGetOnIncorrectType = function (a) {
	return {$: 8, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$getFieldFromList = F2(
	function (selector, fields) {
		getFieldFromList:
		while (true) {
			if (!fields.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var nodifiedTop = fields.a;
				var remain = fields.b;
				var _v1 = $mdgriffith$elm_codegen$Internal$Compiler$denode(nodifiedTop);
				var fieldname = _v1.a;
				var contents = _v1.b;
				if (_Utils_eq(
					$mdgriffith$elm_codegen$Internal$Compiler$denode(fieldname),
					selector)) {
					return $elm$core$Maybe$Just(
						$mdgriffith$elm_codegen$Internal$Compiler$denode(contents));
				} else {
					var $temp$selector = selector,
						$temp$fields = remain;
					selector = $temp$selector;
					fields = $temp$fields;
					continue getFieldFromList;
				}
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$inferRecordField = F2(
	function (index, _v0) {
		var nameOfRecord = _v0.ft;
		var fieldName = _v0.dm;
		var fieldType = $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(
			$mdgriffith$elm_codegen$Internal$Format$formatValue(
				_Utils_ap(
					fieldName,
					$mdgriffith$elm_codegen$Internal$Index$indexToString(index))));
		return $elm$core$Result$Ok(
			{
				iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
				b: A3(
					$mdgriffith$elm_codegen$Internal$Compiler$addInference,
					nameOfRecord,
					A2(
						$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord,
						$mdgriffith$elm_codegen$Internal$Compiler$nodify(nameOfRecord),
						$mdgriffith$elm_codegen$Internal$Compiler$nodify(
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Internal$Compiler$nodify(
									_Utils_Tuple2(
										$mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName),
										$mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldType)))
								]))),
					$elm$core$Dict$empty),
				h$: fieldType
			});
	});
var $mdgriffith$elm_codegen$Internal$Compiler$resolveField = F5(
	function (index, type_, aliases, inferences, fieldName) {
		resolveField:
		while (true) {
			if ($mdgriffith$elm_codegen$Internal$Index$typecheck(index)) {
				switch (type_.$) {
					case 4:
						var fields = type_.a;
						var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$getFieldFromList, fieldName, fields);
						if (!_v1.$) {
							var ann = _v1.a;
							return $elm$core$Result$Ok(
								{iD: aliases, b: inferences, h$: ann});
						} else {
							return $elm$core$Result$Err(
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Internal$Compiler$CouldNotFindField(
										{
											jo: A2(
												$elm$core$List$map,
												A2(
													$elm$core$Basics$composeR,
													$mdgriffith$elm_codegen$Internal$Compiler$denode,
													A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $mdgriffith$elm_codegen$Internal$Compiler$denode)),
												fields),
											w: fieldName
										})
									]));
						}
					case 5:
						var name = type_.a;
						var fields = type_.b;
						var _v2 = A2(
							$mdgriffith$elm_codegen$Internal$Compiler$getFieldFromList,
							fieldName,
							$mdgriffith$elm_codegen$Internal$Compiler$denode(fields));
						if (!_v2.$) {
							var ann = _v2.a;
							return $elm$core$Result$Ok(
								{iD: aliases, b: inferences, h$: ann});
						} else {
							return $elm$core$Result$Err(
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Internal$Compiler$CouldNotFindField(
										{
											jo: A2(
												$elm$core$List$map,
												A2(
													$elm$core$Basics$composeR,
													$mdgriffith$elm_codegen$Internal$Compiler$denode,
													A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $mdgriffith$elm_codegen$Internal$Compiler$denode)),
												$mdgriffith$elm_codegen$Internal$Compiler$denode(fields)),
											w: fieldName
										})
									]));
						}
					case 0:
						var nameOfRecord = type_.a;
						return A2(
							$mdgriffith$elm_codegen$Internal$Compiler$inferRecordField,
							index,
							{dm: fieldName, ft: nameOfRecord});
					case 1:
						var nodedModAndName = type_.a;
						var vars = type_.b;
						var _v3 = A2($mdgriffith$elm_codegen$Internal$Compiler$getAlias, nodedModAndName, aliases);
						if (_v3.$ === 1) {
							return $elm$core$Result$Err(
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Internal$Compiler$AttemptingGetOnTypeNameNotAnAlias(
										{w: fieldName, aa: type_})
									]));
						} else {
							var aliased = _v3.a;
							var $temp$index = index,
								$temp$type_ = aliased.hu,
								$temp$aliases = aliases,
								$temp$inferences = inferences,
								$temp$fieldName = fieldName;
							index = $temp$index;
							type_ = $temp$type_;
							aliases = $temp$aliases;
							inferences = $temp$inferences;
							fieldName = $temp$fieldName;
							continue resolveField;
						}
					case 3:
						return $elm$core$Result$Err(
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Internal$Compiler$AttemptingToGetOnIncorrectType(
									{w: fieldName, aa: type_})
								]));
					case 2:
						return $elm$core$Result$Err(
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Internal$Compiler$AttemptingToGetOnIncorrectType(
									{w: fieldName, aa: type_})
								]));
					default:
						return $elm$core$Result$Err(
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Internal$Compiler$AttemptingToGetOnIncorrectType(
									{w: fieldName, aa: type_})
								]));
				}
			} else {
				return $elm$core$Result$Err(_List_Nil);
			}
		}
	});
var $mdgriffith$elm_codegen$Elm$get = F2(
	function (unformattedFieldName, recordExpression) {
		return function (index) {
			var fieldName = $mdgriffith$elm_codegen$Internal$Format$formatValue(unformattedFieldName);
			var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, recordExpression);
			var expr = _v0.b;
			return {
				bx: function () {
					var _v1 = expr.bx;
					if (!_v1.$) {
						var recordAnn = _v1.a;
						return A5($mdgriffith$elm_codegen$Internal$Compiler$resolveField, index, recordAnn.h$, recordAnn.iD, recordAnn.b, fieldName);
					} else {
						var otherwise = _v1;
						return otherwise;
					}
				}(),
				jr: A2(
					$stil4m$elm_syntax$Elm$Syntax$Expression$RecordAccess,
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(expr.jr),
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName)),
				a: expr.a
			};
		};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$IfBlock = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $mdgriffith$elm_codegen$Elm$ifThen = F3(
	function (condition, thenBranch, elseBranch) {
		return function (index) {
			var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, condition);
			var condIndex = _v0.a;
			var cond = _v0.b;
			var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, condIndex, thenBranch);
			var thenIndex = _v1.a;
			var thenB = _v1.b;
			var _v2 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, thenIndex, elseBranch);
			var finalIndex = _v2.a;
			var elseB = _v2.b;
			return {
				bx: thenB.bx,
				jr: A3(
					$stil4m$elm_syntax$Elm$Syntax$Expression$IfBlock,
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(cond.jr),
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(thenB.jr),
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(elseB.jr)),
				a: _Utils_ap(
					cond.a,
					_Utils_ap(thenB.a, elseB.a))
			};
		};
	});
var $author$project$Interactive$moduleToTabName = function (mod) {
	return A3($elm$core$String$replace, '.', '', mod.ao);
};
var $author$project$Gen$Platform$Cmd$none = $mdgriffith$elm_codegen$Elm$value(
	{
		bx: $elm$core$Maybe$Just(
			A3(
				$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
				_List_Nil,
				'Cmd',
				_List_fromArray(
					[
						$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
					]))),
		ei: _List_fromArray(
			['Platform', 'Cmd']),
		ao: 'none'
	});
var $author$project$Gen$Element$Events$onClick = function (onClickArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attribute',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element', 'Events']),
				ao: 'onClick'
			}),
		_List_fromArray(
			[onClickArg]));
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Integer = function (a) {
	return {$: 7, a: a};
};
var $mdgriffith$elm_codegen$Internal$Types$int = A2(
	$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
	$mdgriffith$elm_codegen$Internal$Types$nodify(
		_Utils_Tuple2(_List_Nil, 'Int')),
	_List_Nil);
var $mdgriffith$elm_codegen$Elm$int = function (intVal) {
	return function (_v0) {
		return {
			bx: $elm$core$Result$Ok(
				{iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, b: $elm$core$Dict$empty, h$: $mdgriffith$elm_codegen$Internal$Types$int}),
			jr: $stil4m$elm_syntax$Elm$Syntax$Expression$Integer(intVal),
			a: _List_Nil
		};
	};
};
var $mdgriffith$elm_codegen$Elm$Annotation$int = A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_Nil, 'Int', _List_Nil);
var $author$project$Gen$Element$paddingXY = F2(
	function (paddingXYArg, paddingXYArg0) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$int, $mdgriffith$elm_codegen$Elm$Annotation$int]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Attribute',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									])))),
					ei: _List_fromArray(
						['Element']),
					ao: 'paddingXY'
				}),
			_List_fromArray(
				[
					$mdgriffith$elm_codegen$Elm$int(paddingXYArg),
					$mdgriffith$elm_codegen$Elm$int(paddingXYArg0)
				]));
	});
var $author$project$Gen$Ui$pointer = $mdgriffith$elm_codegen$Elm$value(
	{
		bx: $elm$core$Maybe$Just(
			A3(
				$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
				_List_fromArray(
					['Element']),
				'Attribute',
				_List_fromArray(
					[
						$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
					]))),
		ei: _List_fromArray(
			['Ui']),
		ao: 'pointer'
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$Floatable = function (a) {
	return {$: 9, a: a};
};
var $mdgriffith$elm_codegen$Internal$Types$float = A2(
	$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
	$mdgriffith$elm_codegen$Internal$Types$nodify(
		_Utils_Tuple2(_List_Nil, 'Float')),
	_List_Nil);
var $mdgriffith$elm_codegen$Elm$float = function (floatVal) {
	return function (_v0) {
		return {
			bx: $elm$core$Result$Ok(
				{iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, b: $elm$core$Dict$empty, h$: $mdgriffith$elm_codegen$Internal$Types$float}),
			jr: $stil4m$elm_syntax$Elm$Syntax$Expression$Floatable(floatVal),
			a: _List_Nil
		};
	};
};
var $mdgriffith$elm_codegen$Elm$Annotation$float = A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_Nil, 'Float', _List_Nil);
var $author$project$Gen$Element$rgb = F3(
	function (rgbArg, rgbArg0, rgbArg1) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$float, $mdgriffith$elm_codegen$Elm$Annotation$float, $mdgriffith$elm_codegen$Elm$Annotation$float]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Color',
								_List_Nil))),
					ei: _List_fromArray(
						['Element']),
					ao: 'rgb'
				}),
			_List_fromArray(
				[
					$mdgriffith$elm_codegen$Elm$float(rgbArg),
					$mdgriffith$elm_codegen$Elm$float(rgbArg0),
					$mdgriffith$elm_codegen$Elm$float(rgbArg1)
				]));
	});
var $author$project$Gen$Element$Border$rounded = function (roundedArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[$mdgriffith$elm_codegen$Elm$Annotation$int]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attribute',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element', 'Border']),
				ao: 'rounded'
			}),
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Elm$int(roundedArg)
			]));
};
var $author$project$Gen$Element$row = F2(
	function (rowArg, rowArg0) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Element']),
										'Attribute',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
											]))),
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Element']),
										'Element',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
											])))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Element',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									])))),
					ei: _List_fromArray(
						['Element']),
					ao: 'row'
				}),
			_List_fromArray(
				[
					$mdgriffith$elm_codegen$Elm$list(rowArg),
					$mdgriffith$elm_codegen$Elm$list(rowArg0)
				]));
	});
var $author$project$Gen$Element$spacing = function (spacingArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[$mdgriffith$elm_codegen$Elm$Annotation$int]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attribute',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element']),
				ao: 'spacing'
			}),
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Elm$int(spacingArg)
			]));
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$Literal = function (a) {
	return {$: 11, a: a};
};
var $mdgriffith$elm_codegen$Internal$Types$string = A2(
	$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
	$mdgriffith$elm_codegen$Internal$Types$nodify(
		_Utils_Tuple2(_List_Nil, 'String')),
	_List_Nil);
var $mdgriffith$elm_codegen$Elm$string = function (literal) {
	return function (_v0) {
		return {
			bx: $elm$core$Result$Ok(
				{iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, b: $elm$core$Dict$empty, h$: $mdgriffith$elm_codegen$Internal$Types$string}),
			jr: $stil4m$elm_syntax$Elm$Syntax$Expression$Literal(literal),
			a: _List_Nil
		};
	};
};
var $mdgriffith$elm_codegen$Elm$Annotation$string = A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_Nil, 'String', _List_Nil);
var $author$project$Gen$Element$text = function (textArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[$mdgriffith$elm_codegen$Elm$Annotation$string]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Element',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element']),
				ao: 'text'
			}),
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Elm$string(textArg)
			]));
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression = function (a) {
	return {$: 13, a: a};
};
var $mdgriffith$elm_codegen$Elm$tuple = F2(
	function (oneExp, twoExp) {
		return function (index) {
			var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, oneExp);
			var oneIndex = _v0.a;
			var one = _v0.b;
			var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, oneIndex, twoExp);
			var twoIndex = _v1.a;
			var two = _v1.b;
			return {
				bx: A3(
					$elm$core$Result$map2,
					F2(
						function (oneA, twoA) {
							return {
								iD: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, twoA.iD, oneA.iD),
								b: A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, twoA.b, oneA.b),
								h$: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Internal$Compiler$nodify(oneA.h$),
											$mdgriffith$elm_codegen$Internal$Compiler$nodify(twoA.h$)
										]))
							};
						}),
					one.bx,
					two.bx),
				jr: $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression(
					_List_fromArray(
						[
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(one.jr),
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(two.jr)
						])),
				a: _Utils_ap(one.a, two.a)
			};
		};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$LetDestructuring = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$LetExpression = function (a) {
	return {$: 15, a: a};
};
var $stil4m$elm_syntax$Elm$Syntax$Expression$RecordUpdateExpression = F2(
	function (a, b) {
		return {$: 22, a: a, b: b};
	});
var $mdgriffith$elm_codegen$Internal$Compiler$RecordUpdateIncorrectFields = function (a) {
	return {$: 11, a: a};
};
var $mdgriffith$elm_codegen$Elm$presentAndMatching = F3(
	function (fieldName, fieldInference, existingFields) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, gathered) {
					var _v1 = _v0.b;
					var _v2 = _v1.a;
					var existingFieldName = _v2.b;
					var _v3 = _v1.b;
					var existingFieldType = _v3.b;
					return gathered ? gathered : (_Utils_eq(fieldName, existingFieldName) ? true : false);
				}),
			false,
			existingFields);
	});
var $mdgriffith$elm_codegen$Elm$verifyFieldsHelper = F2(
	function (existingFields, updatedFields) {
		verifyFieldsHelper:
		while (true) {
			if (!updatedFields.b) {
				return true;
			} else {
				var _v1 = updatedFields.a;
				var fieldName = _v1.a;
				var fieldInference = _v1.b;
				var remain = updatedFields.b;
				if (A3($mdgriffith$elm_codegen$Elm$presentAndMatching, fieldName, fieldInference, existingFields)) {
					var $temp$existingFields = existingFields,
						$temp$updatedFields = remain;
					existingFields = $temp$existingFields;
					updatedFields = $temp$updatedFields;
					continue verifyFieldsHelper;
				} else {
					return false;
				}
			}
		}
	});
var $mdgriffith$elm_codegen$Elm$verifyFields = F2(
	function (updatedFields, existingFields) {
		return A2($mdgriffith$elm_codegen$Elm$verifyFieldsHelper, existingFields, updatedFields) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
			$mdgriffith$elm_codegen$Internal$Compiler$RecordUpdateIncorrectFields(
				{
					iL: A2(
						$elm$core$List$map,
						function (_v0) {
							var fieldName = _v0.a;
							var fieldInference = _v0.b;
							return _Utils_Tuple2(fieldName, fieldInference.h$);
						},
						updatedFields),
					jo: A2(
						$elm$core$List$map,
						function (_v1) {
							var _v2 = _v1.b;
							var _v3 = _v2.a;
							var fieldName = _v3.b;
							var _v4 = _v2.b;
							var fieldInference = _v4.b;
							return _Utils_Tuple2(fieldName, fieldInference);
						},
						existingFields)
				}));
	});
var $mdgriffith$elm_codegen$Elm$updateRecord = F2(
	function (fields, recordExpression) {
		return $mdgriffith$elm_codegen$Internal$Compiler$expression(
			function (index) {
				var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, recordExpression);
				var recordIndex = _v0.a;
				var recordExp = _v0.b;
				var _v1 = A3(
					$elm$core$List$foldl,
					F2(
						function (_v2, _v3) {
							var fieldNameUnformatted = _v2.a;
							var fieldExp = _v2.b;
							var currentIndex = _v3.a;
							var fieldAnnotationResult = _v3.b;
							var items = _v3.c;
							var fieldName = $mdgriffith$elm_codegen$Internal$Format$formatValue(fieldNameUnformatted);
							var _v4 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, currentIndex, fieldExp);
							var newIndex = _v4.a;
							var exp = _v4.b;
							var currentFieldAnnotations = function () {
								if (!fieldAnnotationResult.$) {
									var fieldAnns = fieldAnnotationResult.a;
									var _v6 = exp.bx;
									if (!_v6.$) {
										var fs = _v6.a;
										return $elm$core$Result$Ok(
											A2(
												$elm$core$List$cons,
												_Utils_Tuple2(fieldName, fs),
												fieldAnns));
									} else {
										var newErr = _v6.a;
										return $elm$core$Result$Err(newErr);
									}
								} else {
									var err = fieldAnnotationResult.a;
									var _v7 = exp.bx;
									if (!_v7.$) {
										return fieldAnnotationResult;
									} else {
										var newErr = _v7.a;
										return $elm$core$Result$Err(
											_Utils_ap(err, newErr));
									}
								}
							}();
							return _Utils_Tuple3(
								newIndex,
								currentFieldAnnotations,
								A2(
									$elm$core$List$cons,
									_Utils_Tuple2(fieldName, exp),
									items));
						}),
					_Utils_Tuple3(
						recordIndex,
						$elm$core$Result$Ok(_List_Nil),
						_List_Nil),
					fields);
				var fieldIndex = _v1.a;
				var fieldAnnotationsGathered = _v1.b;
				var fieldDetails = _v1.c;
				return {
					bx: function () {
						if (fieldAnnotationsGathered.$ === 1) {
							var fieldErrors = fieldAnnotationsGathered.a;
							return $elm$core$Result$Err(fieldErrors);
						} else {
							var verifiedFieldAnnotations = fieldAnnotationsGathered.a;
							var _v9 = recordExp.bx;
							if (!_v9.$) {
								var recordAnn = _v9.a;
								var _v10 = recordAnn.h$;
								switch (_v10.$) {
									case 4:
										var existingFields = _v10.a;
										var _v11 = A2($mdgriffith$elm_codegen$Elm$verifyFields, verifiedFieldAnnotations, existingFields);
										if (_v11.$ === 1) {
											return recordExp.bx;
										} else {
											var err = _v11.a;
											return $elm$core$Result$Err(
												_List_fromArray(
													[err]));
										}
									case 0:
										var nameOfRecord = _v10.a;
										return $elm$core$Result$Ok(
											{
												iD: recordAnn.iD,
												b: A3(
													$mdgriffith$elm_codegen$Internal$Compiler$addInference,
													nameOfRecord,
													A2(
														$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericRecord,
														$mdgriffith$elm_codegen$Internal$Compiler$nodify(nameOfRecord),
														$mdgriffith$elm_codegen$Internal$Compiler$nodify(
															A2(
																$elm$core$List$map,
																function (_v12) {
																	var fieldName = _v12.a;
																	var inference = _v12.b;
																	return $mdgriffith$elm_codegen$Internal$Compiler$nodify(
																		_Utils_Tuple2(
																			$mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName),
																			$mdgriffith$elm_codegen$Internal$Compiler$nodify(inference.h$)));
																},
																verifiedFieldAnnotations))),
													recordAnn.b),
												h$: recordAnn.h$
											});
									default:
										var otherwise = _v10;
										return recordExp.bx;
								}
							} else {
								var otherwise = _v9;
								return otherwise;
							}
						}
					}(),
					jr: function () {
						var _v13 = recordExp.jr;
						if (_v13.$ === 3) {
							var name = _v13.b;
							return A2(
								$stil4m$elm_syntax$Elm$Syntax$Expression$RecordUpdateExpression,
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(name),
								A2(
									$elm$core$List$map,
									function (_v14) {
										var fieldName = _v14.a;
										var expDetails = _v14.b;
										return $mdgriffith$elm_codegen$Internal$Compiler$nodify(
											_Utils_Tuple2(
												$mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName),
												$mdgriffith$elm_codegen$Internal$Compiler$nodify(expDetails.jr)));
									},
									$elm$core$List$reverse(fieldDetails)));
						} else {
							var name = 'record' + $mdgriffith$elm_codegen$Internal$Index$indexToString(fieldIndex);
							return $stil4m$elm_syntax$Elm$Syntax$Expression$LetExpression(
								{
									ai: _List_fromArray(
										[
											$mdgriffith$elm_codegen$Internal$Compiler$nodify(
											A2(
												$stil4m$elm_syntax$Elm$Syntax$Expression$LetDestructuring,
												$mdgriffith$elm_codegen$Internal$Compiler$nodify(
													$stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(name)),
												$mdgriffith$elm_codegen$Internal$Compiler$nodify(recordExp.jr)))
										]),
									jr: $mdgriffith$elm_codegen$Internal$Compiler$nodify(
										A2(
											$stil4m$elm_syntax$Elm$Syntax$Expression$RecordUpdateExpression,
											$mdgriffith$elm_codegen$Internal$Compiler$nodify(name),
											A2(
												$elm$core$List$map,
												function (_v15) {
													var fieldName = _v15.a;
													var expDetails = _v15.b;
													return $mdgriffith$elm_codegen$Internal$Compiler$nodify(
														_Utils_Tuple2(
															$mdgriffith$elm_codegen$Internal$Compiler$nodify(fieldName),
															$mdgriffith$elm_codegen$Internal$Compiler$nodify(expDetails.jr)));
												},
												fieldDetails)))
								});
						}
					}(),
					a: A2(
						$elm$core$List$concatMap,
						A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $mdgriffith$elm_codegen$Internal$Compiler$getImports),
						fieldDetails)
				};
			});
	});
var $mdgriffith$elm_codegen$Elm$Variant = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_codegen$Elm$variant = function (name) {
	return A2($mdgriffith$elm_codegen$Elm$Variant, name, _List_Nil);
};
var $mdgriffith$elm_codegen$Elm$variantWith = $mdgriffith$elm_codegen$Elm$Variant;
var $author$project$Gen$Element$Border$width = function (widthArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[$mdgriffith$elm_codegen$Elm$Annotation$int]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attribute',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element', 'Border']),
				ao: 'width'
			}),
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Elm$int(widthArg)
			]));
};
var $author$project$Interactive$codeOrOutput = F2(
	function (top, modules) {
		var variants = {i5: 'ShowCode', fU: 'ShowOutput'};
		var typeName = 'Focus';
		var type_ = A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, typeName);
		var valueNamed = function (name) {
			return $mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(type_),
					ei: _List_Nil,
					ao: name
				});
		};
		var recordName = 'focus_';
		var msgName = 'FocusUpdated';
		return {
			jf: A2(
				$mdgriffith$elm_codegen$Elm$customType,
				typeName,
				_List_fromArray(
					[
						$mdgriffith$elm_codegen$Elm$variant('ShowCode'),
						$mdgriffith$elm_codegen$Elm$variant('ShowOutput')
					])),
			dQ: $mdgriffith$elm_codegen$Elm$get(recordName),
			C: _Utils_Tuple2(
				recordName,
				valueNamed('ShowOutput')),
			_: _Utils_Tuple2(recordName, type_),
			x: A2(
				$mdgriffith$elm_codegen$Elm$variantWith,
				msgName,
				_List_fromArray(
					[
						A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, typeName)
					])),
			hP: A3(
				$mdgriffith$elm_codegen$Elm$Declare$fn,
				'tabToString',
				_Utils_Tuple2(
					'tab',
					$elm$core$Maybe$Just(type_)),
				function (tab) {
					return A3(
						$mdgriffith$elm_codegen$Elm$Case$custom,
						tab,
						type_,
						A2(
							$elm$core$List$map,
							function (mod) {
								return A2(
									$mdgriffith$elm_codegen$Elm$Case$branch0,
									$author$project$Interactive$moduleToTabName(mod),
									$mdgriffith$elm_codegen$Elm$string(mod.ao));
							},
							modules));
				}),
			h$: type_,
			ad: function (model) {
				return A3(
					$mdgriffith$elm_codegen$Elm$Case$branch1,
					msgName,
					_Utils_Tuple2(
						'newTab',
						A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, typeName)),
					function (tab) {
						return A2(
							$mdgriffith$elm_codegen$Elm$tuple,
							A2(
								$mdgriffith$elm_codegen$Elm$updateRecord,
								_List_fromArray(
									[
										_Utils_Tuple2(recordName, tab)
									]),
								model),
							$author$project$Gen$Platform$Cmd$none);
					});
			},
			ij: variants,
			bg: A3(
				$mdgriffith$elm_codegen$Elm$Declare$fn,
				'viewCodeOrResult',
				_Utils_Tuple2(
					'tab',
					$elm$core$Maybe$Just(type_)),
				function (tab) {
					return A2(
						$author$project$Gen$Element$row,
						_List_fromArray(
							[
								$author$project$Gen$Element$spacing(8),
								A2($author$project$Gen$Element$paddingXY, 32, 8),
								$author$project$Gen$Element$alignRight
							]),
						A2(
							$elm$core$List$map,
							function (_v0) {
								var label = _v0.a;
								var varName = _v0.b;
								return A2(
									$author$project$Gen$Element$el,
									_List_fromArray(
										[
											A2($author$project$Gen$Element$paddingXY, 8, 4),
											$author$project$Gen$Element$Border$width(1),
											$author$project$Gen$Element$Border$rounded(4),
											$author$project$Gen$Ui$pointer,
											$author$project$Gen$Element$Border$color(
											A3(
												$mdgriffith$elm_codegen$Elm$ifThen,
												A2(
													$mdgriffith$elm_codegen$Elm$Op$equal,
													tab,
													valueNamed(varName)),
												A3($author$project$Gen$Element$rgb, 1, 1, 1),
												A3($author$project$Gen$Element$rgb, 0, 0, 0))),
											$author$project$Gen$Element$Events$onClick(
											A2(
												$mdgriffith$elm_codegen$Elm$apply,
												$mdgriffith$elm_codegen$Elm$value(
													{
														bx: $elm$core$Maybe$Just(
															A2(
																$mdgriffith$elm_codegen$Elm$Annotation$function,
																_List_fromArray(
																	[type_]),
																$author$project$Interactive$appTypes.x)),
														ei: _List_Nil,
														ao: msgName
													}),
												_List_fromArray(
													[
														$mdgriffith$elm_codegen$Elm$value(
														{
															bx: $elm$core$Maybe$Just(type_),
															ei: _List_Nil,
															ao: varName
														})
													])))
										]),
									$author$project$Gen$Element$text(label));
							},
							_List_fromArray(
								[
									_Utils_Tuple2('Output', 'ShowOutput'),
									_Utils_Tuple2('Example', 'ShowCode')
								])));
				}),
			im: function (model) {
				return A2(
					$mdgriffith$elm_codegen$Elm$apply,
					$mdgriffith$elm_codegen$Elm$value(
						{bx: $elm$core$Maybe$Nothing, ei: _List_Nil, ao: 'viewCodeOrResult'}),
					_List_fromArray(
						[
							A2($mdgriffith$elm_codegen$Elm$get, recordName, model)
						]));
			}
		};
	});
var $mdgriffith$elm_codegen$Internal$Compiler$Exposed = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_codegen$Internal$Compiler$expose = function (decl) {
	switch (decl.$) {
		case 1:
			return decl;
		case 2:
			return decl;
		default:
			var details = decl.a;
			return $mdgriffith$elm_codegen$Internal$Compiler$Declaration(
				_Utils_update(
					details,
					{
						dh: $mdgriffith$elm_codegen$Internal$Compiler$Exposed(
							{jq: false, dV: $elm$core$Maybe$Nothing})
					}));
	}
};
var $mdgriffith$elm_codegen$Elm$expose = $mdgriffith$elm_codegen$Internal$Compiler$expose;
var $author$project$Interactive$toInteractiveInitFields = function (interact) {
	return _Utils_Tuple2(
		interact.ao,
		$mdgriffith$elm_codegen$Elm$record(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var name = _v0.a;
					var info = _v0.b;
					return _Utils_Tuple2(name, info.C);
				},
				interact.aX)));
};
var $author$project$Interactive$toInitFields = function (mod) {
	return A2($elm$core$List$map, $author$project$Interactive$toInteractiveInitFields, mod.c2);
};
var $author$project$Interactive$init = F3(
	function (top, modules, additional) {
		return A2(
			$mdgriffith$elm_codegen$Elm$declaration,
			'init',
			A2(
				$mdgriffith$elm_codegen$Elm$fn,
				_Utils_Tuple2(
					'flags',
					$elm$core$Maybe$Just($mdgriffith$elm_codegen$Elm$Annotation$unit)),
				function (model) {
					return A2(
						$mdgriffith$elm_codegen$Elm$tuple,
						A2(
							$mdgriffith$elm_codegen$Elm$withType,
							$author$project$Interactive$appTypes._,
							$mdgriffith$elm_codegen$Elm$record(
								A2(
									$elm$core$List$cons,
									additional.bc.C,
									A2(
										$elm$core$List$cons,
										additional.aG.C,
										_Utils_ap(
											additional.c1.C,
											A2($elm$core$List$concatMap, $author$project$Interactive$toInitFields, modules)))))),
						$author$project$Gen$Platform$Cmd$none);
				}));
	});
var $author$project$Interactive$logMsg = $mdgriffith$elm_codegen$Elm$variant('Log');
var $author$project$Interactive$capitalize = function (str) {
	var top = A2($elm$core$String$left, 1, str);
	var remain = A2($elm$core$String$dropLeft, 1, str);
	return _Utils_ap(
		$elm$core$String$toUpper(top),
		remain);
};
var $mdgriffith$elm_codegen$Elm$fn2 = F3(
	function (_v0, _v1, toExpression) {
		var oneBaseName = _v0.a;
		var maybeOneType = _v0.b;
		var twoBaseName = _v1.a;
		var maybeTwoType = _v1.b;
		return $mdgriffith$elm_codegen$Internal$Compiler$expression(
			function (index) {
				var one = A3($mdgriffith$elm_codegen$Internal$Compiler$toVarMaybeType, index, oneBaseName, maybeOneType);
				var two = A3($mdgriffith$elm_codegen$Internal$Compiler$toVarMaybeType, one.jK, twoBaseName, maybeTwoType);
				var _v2 = A2(
					$mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails,
					two.jK,
					A2(toExpression, one.g, two.g));
				var newIndex_ = _v2.a;
				var _return = _v2.b;
				return {
					bx: function () {
						var _v3 = _return.bx;
						if (_v3.$ === 1) {
							var err = _v3.a;
							return _return.bx;
						} else {
							var returnAnnotation = _v3.a;
							return $elm$core$Result$Ok(
								{
									iD: returnAnnotation.iD,
									b: returnAnnotation.b,
									h$: A2(
										$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
										$mdgriffith$elm_codegen$Internal$Compiler$nodify(one.h$),
										$mdgriffith$elm_codegen$Internal$Compiler$nodify(
											A2(
												$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$FunctionTypeAnnotation,
												$mdgriffith$elm_codegen$Internal$Compiler$nodify(two.h$),
												$mdgriffith$elm_codegen$Internal$Compiler$nodify(returnAnnotation.h$))))
								});
						}
					}(),
					jr: $stil4m$elm_syntax$Elm$Syntax$Expression$LambdaExpression(
						{
							B: _List_fromArray(
								[
									$mdgriffith$elm_codegen$Internal$Compiler$nodify(
									$stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(one.ao)),
									$mdgriffith$elm_codegen$Internal$Compiler$nodify(
									$stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(two.ao))
								]),
							jr: $mdgriffith$elm_codegen$Internal$Compiler$nodify(_return.jr)
						}),
					a: _return.a
				};
			});
	});
var $mdgriffith$elm_codegen$Elm$Annotation$bool = A3($mdgriffith$elm_codegen$Elm$Annotation$typed, _List_Nil, 'Bool', _List_Nil);
var $mdgriffith$elm_codegen$Elm$Annotation$maybe = function (maybeArg) {
	return A3(
		$mdgriffith$elm_codegen$Elm$Annotation$typed,
		_List_Nil,
		'Maybe',
		_List_fromArray(
			[maybeArg]));
};
var $author$project$Interactive$inputToAnnotation = function (input) {
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
			return $mdgriffith$elm_codegen$Elm$Annotation$maybe(
				$author$project$Interactive$inputToAnnotation(inner));
	}
};
var $author$project$Interactive$fieldsToAnnotation = function (fields) {
	return $mdgriffith$elm_codegen$Elm$Annotation$record(
		A2(
			$elm$core$List$map,
			function (_v0) {
				var name = _v0.a;
				var info = _v0.b;
				return _Utils_Tuple2(
					name,
					$author$project$Interactive$inputToAnnotation(info.jN));
			},
			fields));
};
var $author$project$Interactive$toModelField = function (interact) {
	return _Utils_Tuple2(
		interact.ao,
		$author$project$Interactive$fieldsToAnnotation(interact.aX));
};
var $author$project$Interactive$renderInteractiveViewer = F2(
	function (focus, interact) {
		return A2(
			$mdgriffith$elm_codegen$Elm$declaration,
			'view' + $author$project$Interactive$capitalize(interact.ao),
			A3(
				$mdgriffith$elm_codegen$Elm$fn2,
				_Utils_Tuple2(
					'parent',
					$elm$core$Maybe$Just($author$project$Interactive$appTypes._)),
				_Utils_Tuple2(
					'model',
					$elm$core$Maybe$Just(
						$author$project$Interactive$toModelField(interact).b)),
				F2(
					function (model, submodel) {
						return A2(
							$mdgriffith$elm_codegen$Elm$withType,
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Element',
								_List_fromArray(
									[
										A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, 'Msg')
									])),
							interact.bg(
								{
									i6: A2(
										$mdgriffith$elm_codegen$Elm$Op$equal,
										focus.dQ(model),
										$mdgriffith$elm_codegen$Elm$value(
											{
												bx: $elm$core$Maybe$Just(focus.h$),
												ei: _List_Nil,
												ao: focus.ij.fU
											})),
									_: submodel,
									kb: $mdgriffith$elm_codegen$Elm$value(
										{
											bx: $elm$core$Maybe$Nothing,
											ei: _List_Nil,
											ao: $author$project$Interactive$capitalize(interact.ao)
										})
								}));
					})));
	});
var $author$project$Interactive$renderViewer = F2(
	function (focus, mod) {
		return A2(
			$elm$core$List$map,
			$author$project$Interactive$renderInteractiveViewer(focus),
			mod.c2);
	});
var $author$project$Gen$Element$fill = $mdgriffith$elm_codegen$Elm$value(
	{
		bx: $elm$core$Maybe$Just(
			A3(
				$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
				_List_fromArray(
					['Element']),
				'Length',
				_List_Nil)),
		ei: _List_fromArray(
			['Element']),
		ao: 'fill'
	});
var $author$project$Gen$Element$padding = function (paddingArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[$mdgriffith$elm_codegen$Elm$Annotation$int]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attribute',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element']),
				ao: 'padding'
			}),
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Elm$int(paddingArg)
			]));
};
var $author$project$Gen$Element$width = function (widthArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Length',
								_List_Nil)
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attribute',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element']),
				ao: 'width'
			}),
		_List_fromArray(
			[widthArg]));
};
var $author$project$Interactive$selected = F2(
	function (top, modules) {
		var typeName = 'Selected';
		var type_ = A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, typeName);
		var recordName = 'selectedModule_';
		var msgName = 'TabUpdated';
		return {
			jf: A2(
				$mdgriffith$elm_codegen$Elm$customType,
				typeName,
				A2(
					$elm$core$List$map,
					A2($elm$core$Basics$composeL, $mdgriffith$elm_codegen$Elm$variant, $author$project$Interactive$moduleToTabName),
					modules)),
			C: _Utils_Tuple2(
				recordName,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(type_),
						ei: _List_Nil,
						ao: $author$project$Interactive$moduleToTabName(top)
					})),
			_: _Utils_Tuple2(recordName, type_),
			x: A2(
				$mdgriffith$elm_codegen$Elm$variantWith,
				msgName,
				_List_fromArray(
					[
						A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, typeName)
					])),
			hP: A3(
				$mdgriffith$elm_codegen$Elm$Declare$fn,
				'tabToString',
				_Utils_Tuple2(
					'tab',
					$elm$core$Maybe$Just(type_)),
				function (tab) {
					return A3(
						$mdgriffith$elm_codegen$Elm$Case$custom,
						tab,
						type_,
						A2(
							$elm$core$List$map,
							function (mod) {
								return A2(
									$mdgriffith$elm_codegen$Elm$Case$branch0,
									$author$project$Interactive$moduleToTabName(mod),
									$mdgriffith$elm_codegen$Elm$string(mod.ao));
							},
							modules));
				}),
			h$: type_,
			ad: function (model) {
				return A3(
					$mdgriffith$elm_codegen$Elm$Case$branch1,
					msgName,
					_Utils_Tuple2(
						'newTab',
						A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, typeName)),
					function (tab) {
						return A2(
							$mdgriffith$elm_codegen$Elm$tuple,
							A2(
								$mdgriffith$elm_codegen$Elm$updateRecord,
								_List_fromArray(
									[
										_Utils_Tuple2(recordName, tab)
									]),
								model),
							$author$project$Gen$Platform$Cmd$none);
					});
			},
			bg: A3(
				$mdgriffith$elm_codegen$Elm$Declare$fn,
				'viewTab',
				_Utils_Tuple2(
					'tab',
					$elm$core$Maybe$Just(type_)),
				function (tab) {
					return A2(
						$author$project$Gen$Element$row,
						_List_fromArray(
							[
								$author$project$Gen$Element$spacing(24),
								$author$project$Gen$Element$width($author$project$Gen$Element$fill),
								$author$project$Gen$Element$padding(24)
							]),
						A2(
							$elm$core$List$map,
							function (mod) {
								return A2(
									$author$project$Gen$Element$el,
									_List_fromArray(
										[
											$author$project$Gen$Element$padding(12),
											$author$project$Gen$Element$Events$onClick(
											A2(
												$mdgriffith$elm_codegen$Elm$apply,
												$mdgriffith$elm_codegen$Elm$value(
													{
														bx: $elm$core$Maybe$Just(
															A2(
																$mdgriffith$elm_codegen$Elm$Annotation$function,
																_List_fromArray(
																	[type_]),
																$author$project$Interactive$appTypes.x)),
														ei: _List_Nil,
														ao: msgName
													}),
												_List_fromArray(
													[
														$mdgriffith$elm_codegen$Elm$value(
														{
															bx: $elm$core$Maybe$Just(type_),
															ei: _List_Nil,
															ao: $author$project$Interactive$moduleToTabName(mod)
														})
													])))
										]),
									$author$project$Gen$Element$text(mod.ao));
							},
							modules));
				})
		};
	});
var $mdgriffith$elm_codegen$Elm$bool = function (on) {
	return function (_v0) {
		return {
			bx: $elm$core$Result$Ok(
				{iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, b: $elm$core$Dict$empty, h$: $mdgriffith$elm_codegen$Internal$Types$bool}),
			jr: A2(
				$stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue,
				_List_Nil,
				on ? 'True' : 'False'),
			a: _List_Nil
		};
	};
};
var $mdgriffith$elm_codegen$Elm$val = function (name) {
	return $mdgriffith$elm_codegen$Elm$value(
		{bx: $elm$core$Maybe$Nothing, ei: _List_Nil, ao: name});
};
var $author$project$Interactive$selectedExample = function () {
	var type_ = $mdgriffith$elm_codegen$Elm$Annotation$int;
	var typeName = 'SelectedExample';
	var recordName = 'selectedExample_';
	var msgName = 'SelectedExampleUpdated';
	return {
		dQ: $mdgriffith$elm_codegen$Elm$get(recordName),
		aZ: $mdgriffith$elm_codegen$Elm$get(recordName + '_menu'),
		C: _List_fromArray(
			[
				_Utils_Tuple2(
				recordName,
				$mdgriffith$elm_codegen$Elm$int(0)),
				_Utils_Tuple2(
				recordName + '_menu',
				$mdgriffith$elm_codegen$Elm$bool(false))
			]),
		_: _List_fromArray(
			[
				_Utils_Tuple2(recordName, type_),
				_Utils_Tuple2(recordName + '_menu', $mdgriffith$elm_codegen$Elm$Annotation$bool)
			]),
		fp: _List_fromArray(
			[
				A2(
				$mdgriffith$elm_codegen$Elm$variantWith,
				msgName,
				_List_fromArray(
					[type_])),
				A2(
				$mdgriffith$elm_codegen$Elm$variantWith,
				msgName + '_MenuUpdated',
				_List_fromArray(
					[$mdgriffith$elm_codegen$Elm$Annotation$bool]))
			]),
		fE: function (index) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$int]),
								$author$project$Interactive$appTypes.x)),
						ei: _List_Nil,
						ao: msgName
					}),
				_List_fromArray(
					[
						$mdgriffith$elm_codegen$Elm$int(index)
					]));
		},
		hR: function (current) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$bool]),
								$author$project$Interactive$appTypes.x)),
						ei: _List_Nil,
						ao: msgName + '_MenuUpdated'
					}),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_codegen$Elm$apply,
						$mdgriffith$elm_codegen$Elm$val('not'),
						_List_fromArray(
							[current]))
					]));
		},
		h$: type_,
		ad: function (model) {
			return A3(
				$mdgriffith$elm_codegen$Elm$Case$branch1,
				msgName,
				_Utils_Tuple2('newTab', type_),
				function (tab) {
					return A2(
						$mdgriffith$elm_codegen$Elm$tuple,
						A2(
							$mdgriffith$elm_codegen$Elm$updateRecord,
							_List_fromArray(
								[
									_Utils_Tuple2(recordName, tab)
								]),
							model),
						$author$project$Gen$Platform$Cmd$none);
				});
		},
		ia: function (model) {
			return A3(
				$mdgriffith$elm_codegen$Elm$Case$branch1,
				msgName + '_MenuUpdated',
				_Utils_Tuple2('isOpen', type_),
				function (isOpen) {
					return A2(
						$mdgriffith$elm_codegen$Elm$tuple,
						A2(
							$mdgriffith$elm_codegen$Elm$updateRecord,
							_List_fromArray(
								[
									_Utils_Tuple2(recordName + '_menu', isOpen)
								]),
							model),
						$author$project$Gen$Platform$Cmd$none);
				});
		}
	};
}();
var $author$project$Interactive$toModuleFields = function (mod) {
	return A2($elm$core$List$map, $author$project$Interactive$toModelField, mod.c2);
};
var $author$project$Interactive$toMsgVariantInteractive = function (interact) {
	return A2(
		$mdgriffith$elm_codegen$Elm$variantWith,
		interact.ao,
		_List_fromArray(
			[
				$author$project$Interactive$fieldsToAnnotation(interact.aX)
			]));
};
var $author$project$Interactive$toMsgVariant = function (mod) {
	return A2($elm$core$List$map, $author$project$Interactive$toMsgVariantInteractive, mod.c2);
};
var $author$project$Interactive$logUpdate = function (model) {
	return A2(
		$mdgriffith$elm_codegen$Elm$Case$branch0,
		'Log',
		A2($mdgriffith$elm_codegen$Elm$tuple, model, $author$project$Gen$Platform$Cmd$none));
};
var $author$project$Interactive$toMsgUpdateInteractive = F2(
	function (model, interact) {
		return A3(
			$mdgriffith$elm_codegen$Elm$Case$branch1,
			interact.ao,
			_Utils_Tuple2(
				'updated',
				$author$project$Interactive$fieldsToAnnotation(interact.aX)),
			function (updated) {
				return A2(
					$mdgriffith$elm_codegen$Elm$tuple,
					A2(
						$mdgriffith$elm_codegen$Elm$updateRecord,
						_List_fromArray(
							[
								_Utils_Tuple2(interact.ao, updated)
							]),
						model),
					$author$project$Gen$Platform$Cmd$none);
			});
	});
var $author$project$Interactive$toMsgUpdate = F2(
	function (model, mod) {
		return A2(
			$elm$core$List$map,
			$author$project$Interactive$toMsgUpdateInteractive(model),
			mod.c2);
	});
var $author$project$Interactive$update = F3(
	function (modelAlias, modules, additional) {
		return A2(
			$mdgriffith$elm_codegen$Elm$declaration,
			'update',
			A3(
				$mdgriffith$elm_codegen$Elm$fn2,
				_Utils_Tuple2(
					'msg',
					$elm$core$Maybe$Just(
						A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, 'Msg'))),
				_Utils_Tuple2(
					'model',
					$elm$core$Maybe$Just(modelAlias)),
				F2(
					function (msg, model) {
						return A3(
							$mdgriffith$elm_codegen$Elm$Case$custom,
							msg,
							A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, 'Msg'),
							A2(
								$elm$core$List$cons,
								$author$project$Interactive$logUpdate(model),
								A2(
									$elm$core$List$cons,
									additional.bc.ad(model),
									A2(
										$elm$core$List$cons,
										additional.aG.ad(model),
										A2(
											$elm$core$List$cons,
											additional.c1.ad(model),
											A2(
												$elm$core$List$cons,
												additional.c1.ia(model),
												A2(
													$elm$core$List$concatMap,
													$author$project$Interactive$toMsgUpdate(model),
													modules)))))));
					})));
	});
var $author$project$Gen$Element$below = function (belowArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Element',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									]))
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attribute',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element']),
				ao: 'below'
			}),
		_List_fromArray(
			[belowArg]));
};
var $author$project$Gen$Element$Background$color = function (colorArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Color',
								_List_Nil)
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attr',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('decorative'),
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element', 'Background']),
				ao: 'color'
			}),
		_List_fromArray(
			[colorArg]));
};
var $author$project$Gen$Element$Font$color = function (colorArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Color',
								_List_Nil)
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attr',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('decorative'),
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element', 'Font']),
				ao: 'color'
			}),
		_List_fromArray(
			[colorArg]));
};
var $author$project$Gen$Element$column = F2(
	function (columnArg, columnArg0) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Element']),
										'Attribute',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
											]))),
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Element']),
										'Element',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
											])))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Element',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									])))),
					ei: _List_fromArray(
						['Element']),
					ao: 'column'
				}),
			_List_fromArray(
				[
					$mdgriffith$elm_codegen$Elm$list(columnArg),
					$mdgriffith$elm_codegen$Elm$list(columnArg0)
				]));
	});
var $author$project$Gen$Element$Font$family = function (familyArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								$mdgriffith$elm_codegen$Elm$Annotation$list(
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Element', 'Font']),
									'Font',
									_List_Nil))
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attribute',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element', 'Font']),
				ao: 'family'
			}),
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Elm$list(familyArg)
			]));
};
var $author$project$Gen$Element$height = function (heightArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Length',
								_List_Nil)
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attribute',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element']),
				ao: 'height'
			}),
		_List_fromArray(
			[heightArg]));
};
var $author$project$Gen$Element$htmlAttribute = function (htmlAttributeArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Html']),
								'Attribute',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									]))
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attribute',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element']),
				ao: 'htmlAttribute'
			}),
		_List_fromArray(
			[htmlAttributeArg]));
};
var $author$project$Gen$Element$inFront = function (inFrontArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Element',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									]))
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attribute',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element']),
				ao: 'inFront'
			}),
		_List_fromArray(
			[inFrontArg]));
};
var $author$project$Gen$Element$layout = F2(
	function (layoutArg, layoutArg0) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Element']),
										'Attribute',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
											]))),
									A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Element']),
									'Element',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
										]))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Html']),
								'Html',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									])))),
					ei: _List_fromArray(
						['Element']),
					ao: 'layout'
				}),
			_List_fromArray(
				[
					$mdgriffith$elm_codegen$Elm$list(layoutArg),
					layoutArg0
				]));
	});
var $author$project$Gen$Element$moveRight = function (moveRightArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[$mdgriffith$elm_codegen$Elm$Annotation$float]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attr',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('decorative'),
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element']),
				ao: 'moveRight'
			}),
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Elm$float(moveRightArg)
			]));
};
var $author$project$Gen$Element$none = $mdgriffith$elm_codegen$Elm$value(
	{
		bx: $elm$core$Maybe$Just(
			A3(
				$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
				_List_fromArray(
					['Element']),
				'Element',
				_List_fromArray(
					[
						$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
					]))),
		ei: _List_fromArray(
			['Element']),
		ao: 'none'
	});
var $author$project$Gen$Element$Font$sansSerif = $mdgriffith$elm_codegen$Elm$value(
	{
		bx: $elm$core$Maybe$Just(
			A3(
				$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
				_List_fromArray(
					['Element', 'Font']),
				'Font',
				_List_Nil)),
		ei: _List_fromArray(
			['Element', 'Font']),
		ao: 'sansSerif'
	});
var $author$project$Gen$Element$Font$size = function (sizeArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[$mdgriffith$elm_codegen$Elm$Annotation$int]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element']),
							'Attr',
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('decorative'),
									$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
								])))),
				ei: _List_fromArray(
					['Element', 'Font']),
				ao: 'size'
			}),
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Elm$int(sizeArg)
			]));
};
var $author$project$Gen$Html$Attributes$style = F2(
	function (styleArg, styleArg0) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Html']),
								'Attribute',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									])))),
					ei: _List_fromArray(
						['Html', 'Attributes']),
					ao: 'style'
				}),
			_List_fromArray(
				[
					$mdgriffith$elm_codegen$Elm$string(styleArg),
					$mdgriffith$elm_codegen$Elm$string(styleArg0)
				]));
	});
var $author$project$Gen$Element$Font$typeface = function (typefaceArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[$mdgriffith$elm_codegen$Elm$Annotation$string]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Element', 'Font']),
							'Font',
							_List_Nil))),
				ei: _List_fromArray(
					['Element', 'Font']),
				ao: 'typeface'
			}),
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Elm$string(typefaceArg)
			]));
};
var $author$project$Interactive$view = F3(
	function (modelAlias, modules, additional) {
		return A2(
			$mdgriffith$elm_codegen$Elm$declaration,
			'view',
			A2(
				$mdgriffith$elm_codegen$Elm$fn,
				_Utils_Tuple2(
					'model',
					$elm$core$Maybe$Just(modelAlias)),
				function (model) {
					return A2(
						$mdgriffith$elm_codegen$Elm$withType,
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Html']),
							'Html',
							_List_fromArray(
								[
									A2($mdgriffith$elm_codegen$Elm$Annotation$named, _List_Nil, 'Msg')
								])),
						A2(
							$author$project$Gen$Element$layout,
							_List_fromArray(
								[
									$author$project$Gen$Element$htmlAttribute(
									A2($author$project$Gen$Html$Attributes$style, 'background', 'rgb(36,36,36)')),
									$author$project$Gen$Element$Font$color(
									A3($author$project$Gen$Element$rgb, 1, 1, 1)),
									$author$project$Gen$Element$inFront(
									additional.aG.im(model)),
									$author$project$Gen$Element$Font$family(
									_List_fromArray(
										[
											$author$project$Gen$Element$Font$typeface('Fira Code'),
											$author$project$Gen$Element$Font$sansSerif
										]))
								]),
							A2(
								$author$project$Gen$Element$column,
								_List_fromArray(
									[
										$author$project$Gen$Element$width($author$project$Gen$Element$fill),
										$author$project$Gen$Element$height($author$project$Gen$Element$fill),
										$author$project$Gen$Element$spacing(16)
									]),
								A2(
									$elm$core$List$concatMap,
									function (mod) {
										return A2(
											$elm$core$List$indexedMap,
											F2(
												function (index, interact) {
													return A3(
														$mdgriffith$elm_codegen$Elm$ifThen,
														A2(
															$mdgriffith$elm_codegen$Elm$Op$equal,
															$mdgriffith$elm_codegen$Elm$int(index),
															additional.c1.dQ(model)),
														A2(
															$author$project$Gen$Element$column,
															_List_fromArray(
																[
																	$author$project$Gen$Element$width($author$project$Gen$Element$fill),
																	$author$project$Gen$Element$height($author$project$Gen$Element$fill)
																]),
															_List_fromArray(
																[
																	A2(
																	$author$project$Gen$Element$el,
																	_List_fromArray(
																		[
																			$author$project$Gen$Element$Font$size(24),
																			A2($author$project$Gen$Element$paddingXY, 32, 10),
																			$author$project$Gen$Ui$pointer,
																			$author$project$Gen$Element$Font$family(
																			_List_fromArray(
																				[
																					$author$project$Gen$Element$Font$typeface('Fira Code'),
																					$author$project$Gen$Element$Font$sansSerif
																				])),
																			$author$project$Gen$Element$Events$onClick(
																			additional.c1.hR(
																				additional.c1.aZ(model))),
																			A3(
																			$mdgriffith$elm_codegen$Elm$ifThen,
																			additional.c1.aZ(model),
																			$author$project$Gen$Element$below(
																				A2(
																					$author$project$Gen$Element$column,
																					_List_fromArray(
																						[
																							$author$project$Gen$Element$padding(16),
																							$author$project$Gen$Element$moveRight(32),
																							$author$project$Gen$Element$Border$width(1),
																							$author$project$Gen$Element$Border$rounded(4),
																							$author$project$Gen$Element$Background$color(
																							A3($author$project$Gen$Element$rgb, 0, 0, 0)),
																							$author$project$Gen$Element$spacing(8)
																						]),
																					A2(
																						$elm$core$List$indexedMap,
																						F2(
																							function (optionIndex, option) {
																								return A2(
																									$author$project$Gen$Element$el,
																									_List_fromArray(
																										[
																											$author$project$Gen$Element$Events$onClick(
																											additional.c1.fE(optionIndex))
																										]),
																									$author$project$Gen$Element$text(option.ao));
																							}),
																						mod.c2))),
																			$author$project$Gen$Ui$pointer)
																		]),
																	$author$project$Gen$Element$text('▶ ' + interact.ao)),
																	A2(
																	$mdgriffith$elm_codegen$Elm$apply,
																	$mdgriffith$elm_codegen$Elm$value(
																		{
																			bx: $elm$core$Maybe$Nothing,
																			ei: _List_Nil,
																			ao: 'view' + $author$project$Interactive$capitalize(interact.ao)
																		}),
																	_List_fromArray(
																		[
																			model,
																			A2($mdgriffith$elm_codegen$Elm$get, interact.ao, model)
																		]))
																])),
														$author$project$Gen$Element$none);
												}),
											mod.c2);
									},
									modules))));
				}));
	});
var $author$project$Interactive$generate = F2(
	function (name, modules) {
		if (!modules.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var top = modules.a;
			var remain = modules.b;
			var tab = A2($author$project$Interactive$selected, top, modules);
			var focus = A2($author$project$Interactive$codeOrOutput, top, modules);
			var example = $author$project$Interactive$selectedExample;
			var modelType = $mdgriffith$elm_codegen$Elm$Annotation$record(
				A2(
					$elm$core$List$cons,
					tab._,
					A2(
						$elm$core$List$cons,
						focus._,
						_Utils_ap(
							example._,
							A2($elm$core$List$concatMap, $author$project$Interactive$toModuleFields, modules)))));
			var modelAlias = A4($mdgriffith$elm_codegen$Elm$Annotation$alias, _List_Nil, 'Model', _List_Nil, modelType);
			var additional = {c1: example, aG: focus, bc: tab};
			return $elm$core$Maybe$Just(
				A2(
					$mdgriffith$elm_codegen$Elm$file,
					name,
					$elm$core$List$concat(
						_List_fromArray(
							[
								_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$expose(
									A2($mdgriffith$elm_codegen$Elm$declaration, 'main', $author$project$Interactive$callMain)),
									tab.jf,
									tab.hP.jf,
									focus.jf,
									A2($mdgriffith$elm_codegen$Elm$alias, 'Model', modelType),
									A3($author$project$Interactive$init, top, modules, additional),
									A2(
									$mdgriffith$elm_codegen$Elm$customType,
									'Msg',
									A2(
										$elm$core$List$cons,
										$author$project$Interactive$logMsg,
										A2(
											$elm$core$List$cons,
											focus.x,
											A2(
												$elm$core$List$cons,
												tab.x,
												_Utils_ap(
													example.fp,
													A2($elm$core$List$concatMap, $author$project$Interactive$toMsgVariant, modules)))))),
									A3($author$project$Interactive$update, modelAlias, modules, additional),
									tab.bg.jf,
									focus.bg.jf,
									A3($author$project$Interactive$view, modelAlias, modules, additional)
								]),
								A2(
								$elm$core$List$concatMap,
								$author$project$Interactive$renderViewer(focus),
								modules)
							]))));
		}
	});
var $author$project$Interactive$InputBool = {$: 1};
var $author$project$Interactive$bool = $author$project$Interactive$InputBool;
var $author$project$Example$Interactive$Build$genIdentity = A2(
	$mdgriffith$elm_codegen$Elm$fn,
	_Utils_Tuple2('a', $elm$core$Maybe$Nothing),
	function (a) {
		return a;
	});
var $mdgriffith$elm_codegen$Elm$Op$applyPipe = F4(
	function (_v0, infixAnnotation, l, r) {
		var symbol = _v0.a;
		var dir = _v0.b;
		return function (index) {
			var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, l);
			var leftIndex = _v1.a;
			var left = _v1.b;
			var _v2 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, leftIndex, r);
			var rightIndex = _v2.a;
			var right = _v2.b;
			var annotationIndex = $mdgriffith$elm_codegen$Internal$Index$next(rightIndex);
			return {
				bx: A3(
					$mdgriffith$elm_codegen$Internal$Compiler$applyType,
					index,
					$elm$core$Result$Ok(
						{iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, b: $elm$core$Dict$empty, h$: infixAnnotation}),
					_List_fromArray(
						[left, right])),
				jr: A4(
					$stil4m$elm_syntax$Elm$Syntax$Expression$OperatorApplication,
					symbol,
					dir,
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(left.jr),
					$mdgriffith$elm_codegen$Internal$Compiler$nodify(right.jr)),
				a: _Utils_ap(left.a, right.a)
			};
		};
	});
var $mdgriffith$elm_codegen$Elm$Op$pipe = F2(
	function (r, l) {
		return A4(
			$mdgriffith$elm_codegen$Elm$Op$applyPipe,
			A3($mdgriffith$elm_codegen$Elm$Op$BinOp, '|>', 0, 0),
			A2(
				$mdgriffith$elm_codegen$Internal$Types$function,
				_List_fromArray(
					[
						$mdgriffith$elm_codegen$Internal$Types$var('a'),
						A2(
						$mdgriffith$elm_codegen$Internal$Types$function,
						_List_fromArray(
							[
								$mdgriffith$elm_codegen$Internal$Types$var('a')
							]),
						$mdgriffith$elm_codegen$Internal$Types$var('b'))
					]),
				$mdgriffith$elm_codegen$Internal$Types$var('b')),
			l,
			r);
	});
var $author$project$Example$Interactive$Build$applyBuilder = F2(
	function (_v0, value) {
		var includeBuilder = _v0.a;
		var builder = _v0.b;
		return A2(
			$mdgriffith$elm_codegen$Elm$Op$pipe,
			A3($mdgriffith$elm_codegen$Elm$ifThen, includeBuilder, builder, $author$project$Example$Interactive$Build$genIdentity),
			value);
	});
var $author$project$Interactive$InputFloat = {$: 3};
var $author$project$Interactive$float = $author$project$Interactive$InputFloat;
var $author$project$Example$Type$matchesName = F2(
	function (name, tipe) {
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
var $author$project$Example$Type$isBuilderOfName = F2(
	function (name, tipe) {
		isBuilderOfName:
		while (true) {
			if (tipe.$ === 1) {
				var arg = tipe.a;
				var result = tipe.b;
				if (A2($author$project$Example$Type$matchesName, name, arg) && A2($author$project$Example$Type$matchesName, name, result)) {
					return true;
				} else {
					var $temp$name = name,
						$temp$tipe = result;
					name = $temp$name;
					tipe = $temp$tipe;
					continue isBuilderOfName;
				}
			} else {
				return false;
			}
		}
	});
var $author$project$Example$Type$getBuilderOf = F2(
	function (name, doc) {
		return A2($author$project$Example$Type$isBuilderOfName, name, doc.ay) ? $elm$core$Maybe$Just(doc) : $elm$core$Maybe$Nothing;
	});
var $author$project$Interactive$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Interactive$field = $author$project$Interactive$Field;
var $author$project$Example$Interactive$Build$getVal = F3(
	function (nameBase, options, context) {
		var arg = nameBase;
		return {
			jb: _Utils_update(
				context,
				{
					K: context.K + 1,
					kM: A2(
						$elm$core$List$cons,
						A2($author$project$Interactive$field, arg, options),
						context.kM)
				}),
			cQ: A2($mdgriffith$elm_codegen$Elm$get, arg, context._)
		};
	});
var $author$project$Example$Interactive$Build$getValProtected = F3(
	function (nameBase, options, context) {
		var arg = _Utils_ap(
			nameBase,
			$elm$core$String$fromInt(context.K));
		return {
			jb: _Utils_update(
				context,
				{
					K: context.K + 1,
					kM: A2(
						$elm$core$List$cons,
						A2($author$project$Interactive$field, arg, options),
						context.kM)
				}),
			cQ: A2($mdgriffith$elm_codegen$Elm$get, arg, context._)
		};
	});
var $author$project$Interactive$InputInt = {$: 2};
var $author$project$Interactive$int = $author$project$Interactive$InputInt;
var $author$project$Example$Type$isCreatorOf = F2(
	function (name, tipe) {
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
						var $temp$name = name,
							$temp$tipe = result;
						name = $temp$name;
						tipe = $temp$tipe;
						continue isCreatorOf;
					}
				default:
					return false;
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$getAnnotation = function (exp) {
	return exp.bx;
};
var $mdgriffith$elm_codegen$Elm$maybe = function (maybeContent) {
	return function (index) {
		if (maybeContent.$ === 1) {
			return {
				bx: $elm$core$Result$Ok(
					A2(
						$mdgriffith$elm_codegen$Internal$Compiler$getInnerInference,
						index,
						$mdgriffith$elm_codegen$Elm$Annotation$maybe(
							$mdgriffith$elm_codegen$Elm$Annotation$var('a')))),
				jr: A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, 'Nothing'),
				a: _List_Nil
			};
		} else {
			var contentExp = maybeContent.a;
			var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, contentExp);
			var content = _v1.b;
			return {
				bx: A2(
					$elm$core$Result$map,
					function (ann) {
						return {
							iD: ann.iD,
							b: ann.b,
							h$: A2(
								$stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Typed,
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(
									_Utils_Tuple2(_List_Nil, 'Maybe')),
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Internal$Compiler$nodify(ann.h$)
									]))
						};
					},
					$mdgriffith$elm_codegen$Internal$Compiler$getAnnotation(content)),
				jr: $stil4m$elm_syntax$Elm$Syntax$Expression$Application(
					_List_fromArray(
						[
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(
							A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, 'Just')),
							$mdgriffith$elm_codegen$Internal$Compiler$nodify(
							$stil4m$elm_syntax$Elm$Syntax$Expression$ParenthesizedExpression(
								$mdgriffith$elm_codegen$Internal$Compiler$nodify(content.jr)))
						])),
				a: $mdgriffith$elm_codegen$Internal$Compiler$getImports(content)
			};
		}
	};
};
var $mdgriffith$elm_codegen$Elm$just = function (content) {
	return $mdgriffith$elm_codegen$Elm$maybe(
		$elm$core$Maybe$Just(content));
};
var $author$project$Interactive$log = $mdgriffith$elm_codegen$Elm$value(
	{bx: $elm$core$Maybe$Nothing, ei: _List_Nil, ao: 'Log'});
var $author$project$Interactive$InputString = {$: 0};
var $author$project$Interactive$string = $author$project$Interactive$InputString;
var $elm$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
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
					return $elm$core$Result$Ok(
						A3(func, a, b, c));
				}
			}
		}
	});
var $mdgriffith$elm_codegen$Internal$Compiler$noImports = function (tipe) {
	return {iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, bx: tipe, a: _List_Nil};
};
var $mdgriffith$elm_codegen$Elm$Annotation$triple = F3(
	function (one, two, three) {
		return {
			iD: A2(
				$mdgriffith$elm_codegen$Internal$Compiler$mergeAliases,
				A2(
					$mdgriffith$elm_codegen$Internal$Compiler$mergeAliases,
					$mdgriffith$elm_codegen$Elm$Annotation$getAliases(one),
					$mdgriffith$elm_codegen$Elm$Annotation$getAliases(two)),
				$mdgriffith$elm_codegen$Elm$Annotation$getAliases(three)),
			bx: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Tupled(
				$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
					_List_fromArray(
						[
							$mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(one),
							$mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(two),
							$mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(three)
						]))),
			a: _Utils_ap(
				$mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(one),
				_Utils_ap(
					$mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(two),
					$mdgriffith$elm_codegen$Internal$Compiler$getAnnotationImports(three)))
		};
	});
var $mdgriffith$elm_codegen$Elm$triple = F3(
	function (oneExp, twoExp, threeExp) {
		return function (index) {
			var _v0 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, index, oneExp);
			var oneIndex = _v0.a;
			var one = _v0.b;
			var _v1 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, oneIndex, twoExp);
			var twoIndex = _v1.a;
			var two = _v1.b;
			var _v2 = A2($mdgriffith$elm_codegen$Internal$Compiler$toExpressionDetails, twoIndex, threeExp);
			var threeIndex = _v2.a;
			var three = _v2.b;
			return {
				bx: A4(
					$elm$core$Result$map3,
					F3(
						function (oneA, twoA, threeA) {
							return {
								iD: A2(
									$mdgriffith$elm_codegen$Internal$Compiler$mergeAliases,
									threeA.iD,
									A2($mdgriffith$elm_codegen$Internal$Compiler$mergeAliases, twoA.iD, oneA.iD)),
								b: A2(
									$mdgriffith$elm_codegen$Internal$Compiler$mergeInferences,
									threeA.b,
									A2($mdgriffith$elm_codegen$Internal$Compiler$mergeInferences, twoA.b, oneA.b)),
								h$: $mdgriffith$elm_codegen$Internal$Compiler$getInnerAnnotation(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$triple,
										$mdgriffith$elm_codegen$Internal$Compiler$noImports(oneA.h$),
										$mdgriffith$elm_codegen$Internal$Compiler$noImports(twoA.h$),
										$mdgriffith$elm_codegen$Internal$Compiler$noImports(threeA.h$)))
							};
						}),
					one.bx,
					two.bx,
					three.bx),
				jr: $stil4m$elm_syntax$Elm$Syntax$Expression$TupledExpression(
					$mdgriffith$elm_codegen$Internal$Compiler$nodifyAll(
						_List_fromArray(
							[one.jr, two.jr, three.jr]))),
				a: _Utils_ap(
					one.a,
					_Utils_ap(two.a, three.a))
			};
		};
	});
var $stil4m$elm_syntax$Elm$Syntax$Expression$UnitExpr = {$: 0};
var $mdgriffith$elm_codegen$Internal$Compiler$inference = function (type_) {
	return {iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases, b: $elm$core$Dict$empty, h$: type_};
};
var $mdgriffith$elm_codegen$Elm$unit = function (_v0) {
	return {
		bx: $elm$core$Result$Ok(
			$mdgriffith$elm_codegen$Internal$Compiler$inference($stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$Unit)),
		jr: $stil4m$elm_syntax$Elm$Syntax$Expression$UnitExpr,
		a: _List_Nil
	};
};
var $author$project$Example$Interactive$Build$buildArg = F4(
	function (options, context, namespace, target) {
		_v5$6:
		while (true) {
			_v5$14:
			while (true) {
				switch (target.$) {
					case 0:
						if (target.a === 'msg') {
							return $elm$core$Result$Ok(
								{jb: context, cQ: $author$project$Interactive$log});
						} else {
							var _var = target.a;
							return $elm$core$Result$Err('I don\'t know how to build a ' + _var);
						}
					case 1:
						var arg = target.a;
						var result = target.b;
						return $elm$core$Result$Err('Nested lambdas');
					case 2:
						if (!target.a.b) {
							return $elm$core$Result$Ok(
								{jb: context, cQ: $mdgriffith$elm_codegen$Elm$unit});
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
										var _v9 = A4($author$project$Example$Interactive$Build$buildArg, options, oneBuilt.jb, namespace, two);
										if (!_v9.$) {
											var twoBuilt = _v9.a;
											return $elm$core$Result$Ok(
												{
													jb: twoBuilt.jb,
													cQ: A2($mdgriffith$elm_codegen$Elm$tuple, oneBuilt.cQ, twoBuilt.cQ)
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
											var _v14 = A4($author$project$Example$Interactive$Build$buildArg, options, oneBuilt.jb, namespace, two);
											if (!_v14.$) {
												var twoBuilt = _v14.a;
												var _v15 = A4($author$project$Example$Interactive$Build$buildArg, options, twoBuilt.jb, namespace, three);
												if (!_v15.$) {
													var threeBuilt = _v15.a;
													return $elm$core$Result$Ok(
														{
															jb: threeBuilt.jb,
															cQ: A3($mdgriffith$elm_codegen$Elm$triple, oneBuilt.cQ, twoBuilt.cQ, threeBuilt.cQ)
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
								case 'String.String':
									return $elm$core$Result$Ok(
										A3(
											$author$project$Example$Interactive$Build$getVal,
											namespace,
											{
												C: $mdgriffith$elm_codegen$Elm$string('Button'),
												jN: $author$project$Interactive$string
											},
											context));
								case 'Basics.Boolean':
									return $elm$core$Result$Ok(
										A3(
											$author$project$Example$Interactive$Build$getVal,
											namespace,
											{
												C: $mdgriffith$elm_codegen$Elm$bool(true),
												jN: $author$project$Interactive$bool
											},
											context));
								case 'Basics.Int':
									return $elm$core$Result$Ok(
										A3(
											$author$project$Example$Interactive$Build$getVal,
											namespace,
											{
												C: $mdgriffith$elm_codegen$Elm$int(1),
												jN: $author$project$Interactive$int
											},
											context));
								case 'Basics.Float':
									return $elm$core$Result$Ok(
										A3(
											$author$project$Example$Interactive$Build$getVal,
											namespace,
											{
												C: $mdgriffith$elm_codegen$Elm$float(1),
												jN: $author$project$Interactive$float
											},
											context));
								case 'Basics.Bool':
									return $elm$core$Result$Ok(
										A3(
											$author$project$Example$Interactive$Build$getVal,
											namespace,
											{
												C: $mdgriffith$elm_codegen$Elm$bool(true),
												jN: $author$project$Interactive$bool
											},
											context));
								default:
									break _v5$14;
							}
						} else {
							if (!target.b.b.b) {
								switch (target.a) {
									case 'Maybe.Maybe':
										var _v16 = target.b;
										var inner = _v16.a;
										var _v17 = A4($author$project$Example$Interactive$Build$buildArg, options, context, namespace, inner);
										if (_v17.$ === 1) {
											var err = _v17.a;
											return $elm$core$Result$Err(err);
										} else {
											var innerExample = _v17.a;
											return $elm$core$Result$Ok(
												{
													jb: innerExample.jb,
													cQ: $mdgriffith$elm_codegen$Elm$just(innerExample.cQ)
												});
										}
									case 'List.List':
										var _v18 = target.b;
										var inner = _v18.a;
										var _v19 = A4($author$project$Example$Interactive$Build$buildArg, options, context, namespace, inner);
										if (_v19.$ === 1) {
											var err = _v19.a;
											return $elm$core$Result$Err(err);
										} else {
											var innerExample = _v19.a;
											return $elm$core$Result$Ok(
												{
													jb: innerExample.jb,
													cQ: $mdgriffith$elm_codegen$Elm$list(
														_List_fromArray(
															[innerExample.cQ]))
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
						var renderedResult = A3(
							$elm$core$List$foldl,
							F2(
								function (_v29, gathered) {
									var fieldName = _v29.a;
									var fieldType = _v29.b;
									if (gathered.$ === 1) {
										var err = gathered.a;
										return gathered;
									} else {
										var _v31 = gathered.a;
										var currentContext = _v31.a;
										var renderedFields = _v31.b;
										var _v32 = A4($author$project$Example$Interactive$Build$buildArg, options, currentContext, fieldName, fieldType);
										if (!_v32.$) {
											var fieldExample = _v32.a;
											return $elm$core$Result$Ok(
												_Utils_Tuple2(
													fieldExample.jb,
													A2(
														$elm$core$List$cons,
														_Utils_Tuple2(fieldName, fieldExample.cQ),
														renderedFields)));
										} else {
											var err = _v32.a;
											return $elm$core$Result$Err(err);
										}
									}
								}),
							$elm$core$Result$Ok(
								_Utils_Tuple2(context, _List_Nil)),
							fields);
						if (!renderedResult.$) {
							var _v28 = renderedResult.a;
							var newContext = _v28.a;
							var rendered = _v28.b;
							return $elm$core$Result$Ok(
								{
									jb: newContext,
									cQ: $mdgriffith$elm_codegen$Elm$record(rendered)
								});
						} else {
							var err = renderedResult.a;
							return $elm$core$Result$Err(err);
						}
				}
			}
			var name = target.a;
			var vars = target.b;
			return A3(
				$elm$core$List$foldl,
				F2(
					function (decl, buildResult) {
						if (!buildResult.$) {
							return buildResult;
						} else {
							if (A2($author$project$Example$Type$isCreatorOf, name, decl.ay)) {
								if (options.ag) {
									var _v21 = A3(
										$elm$core$List$foldl,
										F2(
											function (doc, untouched) {
												var ctxt = untouched.a;
												var existingBuilders = untouched.b;
												var _v22 = A2($author$project$Example$Type$getBuilderOf, name, doc);
												if (_v22.$ === 1) {
													return untouched;
												} else {
													var builder = _v22.a;
													var builtBuilderResult = A5(
														$author$project$Example$Interactive$Build$buildBuilder,
														{ag: false},
														ctxt,
														builder,
														builder.ay,
														_List_Nil);
													if (builtBuilderResult.$ === 1) {
														return untouched;
													} else {
														var builtBuilder = builtBuilderResult.a;
														var builderSwitch = A3(
															$author$project$Example$Interactive$Build$getValProtected,
															'includeBuilder',
															{
																C: $mdgriffith$elm_codegen$Elm$bool(false),
																jN: $author$project$Interactive$bool
															},
															builtBuilder.jb);
														return _Utils_Tuple2(
															builderSwitch.jb,
															A2(
																$elm$core$List$cons,
																_Utils_Tuple2(builderSwitch.cQ, builtBuilder.cQ),
																existingBuilders));
													}
												}
											}),
										_Utils_Tuple2(context, _List_Nil),
										context.A.k$);
									var buildersContext = _v21.a;
									var builders = _v21.b;
									var exampleCall = A5(
										$author$project$Example$Interactive$Build$buildExampleCall,
										{ag: false},
										buildersContext,
										{
											aV: function (_v25) {
												return true;
											},
											t: decl
										},
										decl.ay,
										_List_Nil);
									if (!exampleCall.$) {
										var builtValue = exampleCall.a;
										return $elm$core$Result$Ok(
											{
												jb: builtValue.jb,
												cQ: A3($elm$core$List$foldl, $author$project$Example$Interactive$Build$applyBuilder, builtValue.cQ, builders)
											});
									} else {
										var err = exampleCall.a;
										return $elm$core$Result$Err(err);
									}
								} else {
									return A5(
										$author$project$Example$Interactive$Build$buildExampleCall,
										{ag: false},
										context,
										{
											aV: function (_v26) {
												return true;
											},
											t: decl
										},
										decl.ay,
										_List_Nil);
								}
							} else {
								return buildResult;
							}
						}
					}),
				$elm$core$Result$Err('I don\'t know how to build a ' + name),
				context.A.k$);
		}
		return $elm$core$Result$Err('I don\'t know how to build a tuple with values other than a 0, 2, and three.');
	});
var $author$project$Example$Interactive$Build$buildBuilder = F5(
	function (options, context, originalValue, targetType, args) {
		buildBuilder:
		while (true) {
			if (targetType.$ === 1) {
				if (targetType.b.$ === 1) {
					var arg = targetType.a;
					var result = targetType.b;
					var _v4 = A4($author$project$Example$Interactive$Build$buildArg, options, context, originalValue.ao, arg);
					if (!_v4.$) {
						var argBuilt = _v4.a;
						var $temp$options = options,
							$temp$context = argBuilt.jb,
							$temp$originalValue = originalValue,
							$temp$targetType = result,
							$temp$args = A2($elm$core$List$cons, argBuilt.cQ, args);
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
					return $elm$core$Result$Ok(
						{
							jb: context,
							cQ: A2(
								$mdgriffith$elm_codegen$Elm$apply,
								$mdgriffith$elm_codegen$Elm$value(
									{
										bx: $elm$core$Maybe$Nothing,
										ei: A2($elm$core$String$split, '.', context.A.ao),
										ao: originalValue.ao
									}),
								$elm$core$List$reverse(args))
						});
				}
			} else {
				return A4($author$project$Example$Interactive$Build$buildArg, options, context, originalValue.ao, targetType);
			}
		}
	});
var $author$project$Example$Interactive$Build$buildExampleCall = F5(
	function (options, context, bounds, targetType, args) {
		buildExampleCall:
		while (true) {
			if (targetType.$ === 1) {
				var arg = targetType.a;
				var result = targetType.b;
				var _v1 = A4($author$project$Example$Interactive$Build$buildArg, options, context, bounds.t.ao, arg);
				if (!_v1.$) {
					var argBuilt = _v1.a;
					if (result.$ === 1) {
						var $temp$options = options,
							$temp$context = argBuilt.jb,
							$temp$bounds = bounds,
							$temp$targetType = result,
							$temp$args = A2($elm$core$List$cons, argBuilt.cQ, args);
						options = $temp$options;
						context = $temp$context;
						bounds = $temp$bounds;
						targetType = $temp$targetType;
						args = $temp$args;
						continue buildExampleCall;
					} else {
						return $elm$core$Result$Ok(
							{
								jb: argBuilt.jb,
								cQ: A2(
									$mdgriffith$elm_codegen$Elm$apply,
									$mdgriffith$elm_codegen$Elm$value(
										{
											bx: $elm$core$Maybe$Nothing,
											ei: A2($elm$core$String$split, '.', argBuilt.jb.A.ao),
											ao: bounds.t.ao
										}),
									$elm$core$List$reverse(
										A2($elm$core$List$cons, argBuilt.cQ, args)))
							});
					}
				} else {
					var err = _v1.a;
					return $elm$core$Result$Err(err);
				}
			} else {
				return $elm$core$Result$Ok(
					{
						jb: context,
						cQ: $mdgriffith$elm_codegen$Elm$value(
							{
								bx: $elm$core$Maybe$Nothing,
								ei: A2($elm$core$String$split, '.', context.A.ao),
								ao: bounds.t.ao
							})
					});
			}
		}
	});
var $author$project$Interactive$fromType = function (tipe) {
	_v0$4:
	while (true) {
		if ((tipe.$ === 3) && (!tipe.b.b)) {
			switch (tipe.a) {
				case 'String.String':
					return $elm$core$Maybe$Just(
						{
							C: $mdgriffith$elm_codegen$Elm$string(''),
							jN: $author$project$Interactive$InputString
						});
				case 'Basics.Bool':
					return $elm$core$Maybe$Just(
						{
							C: $mdgriffith$elm_codegen$Elm$bool(false),
							jN: $author$project$Interactive$InputBool
						});
				case 'Basics.Int':
					return $elm$core$Maybe$Just(
						{
							C: $mdgriffith$elm_codegen$Elm$int(0),
							jN: $author$project$Interactive$InputInt
						});
				case 'Basics.Float':
					return $elm$core$Maybe$Just(
						{
							C: $mdgriffith$elm_codegen$Elm$float(0),
							jN: $author$project$Interactive$InputFloat
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
var $author$project$Example$Type$getArgsHelper = F2(
	function (tipe, found) {
		getArgsHelper:
		while (true) {
			if (tipe.$ === 1) {
				var arg = tipe.a;
				var result = tipe.b;
				var $temp$tipe = result,
					$temp$found = A2($elm$core$List$cons, arg, found);
				tipe = $temp$tipe;
				found = $temp$found;
				continue getArgsHelper;
			} else {
				return $elm$core$List$reverse(found);
			}
		}
	});
var $author$project$Example$Type$getArgs = function (tipe) {
	return A2($author$project$Example$Type$getArgsHelper, tipe, _List_Nil);
};
var $mdgriffith$elm_codegen$Internal$Compiler$var = F2(
	function (index, name) {
		var protectedName = $mdgriffith$elm_codegen$Internal$Format$sanitize(
			_Utils_ap(
				name,
				$mdgriffith$elm_codegen$Internal$Index$indexToString(index)));
		return _Utils_Tuple3(
			$mdgriffith$elm_codegen$Internal$Index$next(index),
			protectedName,
			function (existingIndex_) {
				return {
					bx: $elm$core$Result$Ok(
						{
							iD: $mdgriffith$elm_codegen$Internal$Compiler$emptyAliases,
							b: $elm$core$Dict$empty,
							h$: $stil4m$elm_syntax$Elm$Syntax$TypeAnnotation$GenericType(
								$mdgriffith$elm_codegen$Internal$Format$formatValue(
									_Utils_ap(
										name,
										$mdgriffith$elm_codegen$Internal$Index$indexToString(existingIndex_))))
						}),
					jr: A2($stil4m$elm_syntax$Elm$Syntax$Expression$FunctionOrValue, _List_Nil, protectedName),
					a: _List_Nil
				};
			});
	});
var $mdgriffith$elm_codegen$Elm$Case$maybe = F2(
	function (mainExpression, branches) {
		return function (index) {
			var _v0 = A4(
				$mdgriffith$elm_codegen$Elm$Case$captureCase,
				mainExpression,
				_List_Nil,
				index,
				_List_fromArray(
					[
						function (branchIndex) {
						return _Utils_Tuple3(
							branchIndex,
							A2(
								$stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern,
								{a1: _List_Nil, ao: 'Nothing'},
								_List_Nil),
							branches.j8);
					},
						function (branchIndex) {
						var _v1 = branches.jS;
						var justVarName = _v1.a;
						var toReturn = _v1.b;
						var _v2 = A2($mdgriffith$elm_codegen$Internal$Compiler$var, branchIndex, justVarName);
						var justIndex = _v2.a;
						var justName = _v2.b;
						var justExp = _v2.c;
						return _Utils_Tuple3(
							justIndex,
							A2(
								$stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern,
								{a1: _List_Nil, ao: 'Just'},
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Internal$Compiler$nodify(
										$stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(justName))
									])),
							toReturn(justExp));
					}
					]));
			var expr = _v0.a;
			var gathered = _v0.b;
			return {
				bx: function () {
					var _v3 = gathered.bx;
					if (_v3.$ === 1) {
						return $elm$core$Result$Err(
							_List_fromArray(
								[$mdgriffith$elm_codegen$Internal$Compiler$EmptyCaseStatement]));
					} else {
						var ann = _v3.a;
						return ann;
					}
				}(),
				jr: $stil4m$elm_syntax$Elm$Syntax$Expression$CaseExpression(
					{
						j: $elm$core$List$reverse(gathered.j),
						jr: $mdgriffith$elm_codegen$Internal$Compiler$nodify(expr.jr)
					}),
				a: _Utils_ap(expr.a, gathered.a)
			};
		};
	});
var $author$project$Interactive$InputMaybe = function (a) {
	return {$: 4, a: a};
};
var $author$project$Interactive$maybe = $author$project$Interactive$InputMaybe;
var $author$project$Example$CallStack$name = function (_v0) {
	var call = _v0;
	return call.t.ao;
};
var $mdgriffith$elm_codegen$Elm$nothing = $mdgriffith$elm_codegen$Elm$maybe($elm$core$Maybe$Nothing);
var $author$project$Example$CallStack$start = function (_v0) {
	var call = _v0;
	return call.t;
};
var $author$project$Example$Interactive$Build$buildHelper = F3(
	function (options, context, _v0) {
		var callstack = _v0;
		var starterCall = options.a5 ? A5(
			$author$project$Example$Interactive$Build$buildBuilder,
			{ag: options.ag},
			context,
			callstack.t,
			callstack.t.ay,
			_List_Nil) : A5(
			$author$project$Example$Interactive$Build$buildExampleCall,
			{ag: options.ag},
			context,
			{
				aV: function (_v7) {
					return true;
				},
				t: callstack.t
			},
			callstack.t.ay,
			_List_Nil);
		if (!starterCall.$) {
			var call = starterCall.a;
			return A3(
				$elm$core$List$foldl,
				F2(
					function (step, builtResult) {
						if (!builtResult.$) {
							var built = builtResult.a;
							if (step.a8) {
								var _v3 = A3(
									$author$project$Example$Interactive$Build$buildHelper,
									_Utils_update(
										options,
										{a5: true}),
									built.jb,
									step.ba);
								if (!_v3.$) {
									var builtStep = _v3.a;
									return $elm$core$Result$Ok(
										{
											jb: builtStep.jb,
											cQ: A2($mdgriffith$elm_codegen$Elm$Op$pipe, builtStep.cQ, built.cQ)
										});
								} else {
									var err = _v3.a;
									return $elm$core$Result$Err(err);
								}
							} else {
								var _v4 = $author$project$Example$Type$getArgs(
									$author$project$Example$CallStack$start(step.ba).ay);
								_v4$2:
								while (true) {
									if (_v4.b) {
										if (!_v4.b.b) {
											var boolVal = A3(
												$author$project$Example$Interactive$Build$getVal,
												$author$project$Example$CallStack$name(step.ba),
												{
													C: $mdgriffith$elm_codegen$Elm$bool(false),
													jN: $author$project$Interactive$bool
												},
												built.jb);
											return $elm$core$Result$Ok(
												{
													jb: boolVal.jb,
													cQ: A2(
														$mdgriffith$elm_codegen$Elm$Op$pipe,
														A3(
															$mdgriffith$elm_codegen$Elm$ifThen,
															boolVal.cQ,
															$mdgriffith$elm_codegen$Elm$value(
																{
																	bx: $elm$core$Maybe$Nothing,
																	ei: A2($elm$core$String$split, '.', context.A.ao),
																	ao: $author$project$Example$CallStack$name(step.ba)
																}),
															$author$project$Example$Interactive$Build$genIdentity),
														built.cQ)
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
													var maybeVal = A3(
														$author$project$Example$Interactive$Build$getVal,
														$author$project$Example$CallStack$name(step.ba),
														{
															C: $mdgriffith$elm_codegen$Elm$nothing,
															jN: $author$project$Interactive$maybe(input.jN)
														},
														built.jb);
													return $elm$core$Result$Ok(
														{
															jb: maybeVal.jb,
															cQ: A2(
																$mdgriffith$elm_codegen$Elm$Op$pipe,
																A2(
																	$mdgriffith$elm_codegen$Elm$Case$maybe,
																	maybeVal.cQ,
																	{
																		jS: _Utils_Tuple2(
																			$author$project$Example$CallStack$name(step.ba) + '_option',
																			function (val) {
																				return A2(
																					$mdgriffith$elm_codegen$Elm$apply,
																					$mdgriffith$elm_codegen$Elm$value(
																						{
																							bx: $elm$core$Maybe$Nothing,
																							ei: A2($elm$core$String$split, '.', context.A.ao),
																							ao: $author$project$Example$CallStack$name(step.ba)
																						}),
																					_List_fromArray(
																						[val]));
																			}),
																		j8: $author$project$Example$Interactive$Build$genIdentity
																	}),
																built.cQ)
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
							var err = builtResult.a;
							return $elm$core$Result$Err(err);
						}
					}),
				$elm$core$Result$Ok(call),
				$elm$core$List$reverse(callstack.aM));
		} else {
			var err = starterCall.a;
			return $elm$core$Result$Err(err);
		}
	});
var $author$project$Example$Interactive$Build$initContext = function (modul) {
	return {
		K: 0,
		_: $mdgriffith$elm_codegen$Elm$value(
			{bx: $elm$core$Maybe$Nothing, ei: _List_Nil, ao: 'model'}),
		A: modul,
		kM: _List_Nil
	};
};
var $author$project$Example$Interactive$Build$build = F2(
	function (mod, callstack) {
		return A3(
			$author$project$Example$Interactive$Build$buildHelper,
			{ag: true, a5: false},
			$author$project$Example$Interactive$Build$initContext(mod),
			callstack);
	});
var $author$project$Gen$Elm$apply = F2(
	function (applyArg, applyArg0) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil),
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'apply'
				}),
			_List_fromArray(
				[
					applyArg,
					$mdgriffith$elm_codegen$Elm$list(applyArg0)
				]));
	});
var $author$project$Example$Interactive$Rendered$genIdentity = A2(
	$mdgriffith$elm_codegen$Elm$fn,
	_Utils_Tuple2('a', $elm$core$Maybe$Nothing),
	function (a) {
		return a;
	});
var $author$project$Example$Interactive$Rendered$applyBuilder = F2(
	function (_v0, value) {
		var includeBuilder = _v0.a;
		var builder = _v0.b;
		return A2(
			$mdgriffith$elm_codegen$Elm$Op$pipe,
			A3($mdgriffith$elm_codegen$Elm$ifThen, includeBuilder, builder, $author$project$Example$Interactive$Rendered$genIdentity),
			value);
	});
var $mdgriffith$elm_codegen$Elm$Annotation$char = A3(
	$mdgriffith$elm_codegen$Elm$Annotation$typed,
	_List_fromArray(
		['Char']),
	'Char',
	_List_Nil);
var $author$project$Gen$Elm$call_ = {
	br: F2(
		function (aliasArg, aliasArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm', 'Annotation']),
										'Annotation',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Declaration',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'alias'
					}),
				_List_fromArray(
					[aliasArg, aliasArg0]));
		}),
	bB: F2(
		function (applyArg, applyArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil),
										$mdgriffith$elm_codegen$Elm$Annotation$list(
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Expression',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'apply'
					}),
				_List_fromArray(
					[applyArg, applyArg0]));
		}),
	iS: function (boolArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$bool]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'bool'
				}),
			_List_fromArray(
				[boolArg]));
	},
	b$: function (charArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$char]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'char'
				}),
			_List_fromArray(
				[charArg]));
	},
	cd: function (commentArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Declaration',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'comment'
				}),
			_List_fromArray(
				[commentArg]));
	},
	cn: F2(
		function (customTypeArg, customTypeArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$list(
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Variant',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Declaration',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'customType'
					}),
				_List_fromArray(
					[customTypeArg, customTypeArg0]));
		}),
	jf: F2(
		function (declarationArg, declarationArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Declaration',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'declaration'
					}),
				_List_fromArray(
					[declarationArg, declarationArg0]));
		}),
	aT: function (docsArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$record(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'group',
											$mdgriffith$elm_codegen$Elm$Annotation$maybe($mdgriffith$elm_codegen$Elm$Annotation$string)),
											_Utils_Tuple2(
											'members',
											$mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string))
										]))
								]),
							$mdgriffith$elm_codegen$Elm$Annotation$string)),
					ei: _List_fromArray(
						['Elm']),
					ao: 'docs'
				}),
			_List_fromArray(
				[docsArg]));
	},
	df: function (exposeArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Declaration',
									_List_Nil)
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Declaration',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'expose'
				}),
			_List_fromArray(
				[exposeArg]));
	},
	dg: F2(
		function (exposeWithArg, exposeWithArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$record(
										_List_fromArray(
											[
												_Utils_Tuple2('exposeConstructor', $mdgriffith$elm_codegen$Elm$Annotation$bool),
												_Utils_Tuple2(
												'group',
												$mdgriffith$elm_codegen$Elm$Annotation$maybe($mdgriffith$elm_codegen$Elm$Annotation$string))
											])),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Declaration',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Declaration',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'exposeWith'
					}),
				_List_fromArray(
					[exposeWithArg, exposeWithArg0]));
		}),
	aF: F2(
		function (fileArg, fileArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string),
										$mdgriffith$elm_codegen$Elm$Annotation$list(
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Declaration',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'File',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'file'
					}),
				_List_fromArray(
					[fileArg, fileArg0]));
		}),
	dq: F3(
		function (fileWithArg, fileWithArg0, fileWithArg1) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string),
										$mdgriffith$elm_codegen$Elm$Annotation$record(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'docs',
												A2(
													$mdgriffith$elm_codegen$Elm$Annotation$function,
													_List_fromArray(
														[
															$mdgriffith$elm_codegen$Elm$Annotation$list(
															$mdgriffith$elm_codegen$Elm$Annotation$record(
																_List_fromArray(
																	[
																		_Utils_Tuple2(
																		'group',
																		$mdgriffith$elm_codegen$Elm$Annotation$maybe($mdgriffith$elm_codegen$Elm$Annotation$string)),
																		_Utils_Tuple2(
																		'members',
																		$mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string))
																	])))
														]),
													$mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string))),
												_Utils_Tuple2(
												'aliases',
												$mdgriffith$elm_codegen$Elm$Annotation$list(
													A2(
														$mdgriffith$elm_codegen$Elm$Annotation$tuple,
														$mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string),
														$mdgriffith$elm_codegen$Elm$Annotation$string)))
											])),
										$mdgriffith$elm_codegen$Elm$Annotation$list(
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Declaration',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'File',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'fileWith'
					}),
				_List_fromArray(
					[fileWithArg, fileWithArg0, fileWithArg1]));
		}),
	jx: function (floatArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$float]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'float'
				}),
			_List_fromArray(
				[floatArg]));
	},
	dt: F2(
		function (fnArg, fnArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil)
											]),
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Expression',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'fn'
					}),
				_List_fromArray(
					[fnArg, fnArg0]));
		}),
	du: F3(
		function (fn2Arg, fn2Arg0, fn2Arg1) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil)
											]),
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Expression',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'fn2'
					}),
				_List_fromArray(
					[fn2Arg, fn2Arg0, fn2Arg1]));
		}),
	dv: F4(
		function (fn3Arg, fn3Arg0, fn3Arg1, fn3Arg2) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil)
											]),
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Expression',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'fn3'
					}),
				_List_fromArray(
					[fn3Arg, fn3Arg0, fn3Arg1, fn3Arg2]));
		}),
	dw: F5(
		function (fn4Arg, fn4Arg0, fn4Arg1, fn4Arg2, fn4Arg3) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil)
											]),
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Expression',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'fn4'
					}),
				_List_fromArray(
					[fn4Arg, fn4Arg0, fn4Arg1, fn4Arg2, fn4Arg3]));
		}),
	dx: F6(
		function (fn5Arg, fn5Arg0, fn5Arg1, fn5Arg2, fn5Arg3, fn5Arg4) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil)
											]),
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Expression',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'fn5'
					}),
				_List_fromArray(
					[fn5Arg, fn5Arg0, fn5Arg1, fn5Arg2, fn5Arg3, fn5Arg4]));
		}),
	dy: F7(
		function (fn6Arg, fn6Arg0, fn6Arg1, fn6Arg2, fn6Arg3, fn6Arg4, fn6Arg5) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil),
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil)
											]),
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Expression',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'fn6'
					}),
				_List_fromArray(
					[fn6Arg, fn6Arg0, fn6Arg1, fn6Arg2, fn6Arg3, fn6Arg4, fn6Arg5]));
		}),
	dN: F2(
		function (functionArg, functionArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$list(
										A2(
											$mdgriffith$elm_codegen$Elm$Annotation$tuple,
											$mdgriffith$elm_codegen$Elm$Annotation$string,
											$mdgriffith$elm_codegen$Elm$Annotation$maybe(
												A3(
													$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
													_List_fromArray(
														['Elm', 'Annotation']),
													'Annotation',
													_List_Nil)))),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$list(
												A3(
													$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
													_List_fromArray(
														['Elm']),
													'Expression',
													_List_Nil))
											]),
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Expression',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'function'
					}),
				_List_fromArray(
					[functionArg, functionArg0]));
		}),
	dO: F2(
		function (functionReducedArg, functionReducedArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil)
											]),
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Expression',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'functionReduced'
					}),
				_List_fromArray(
					[functionReducedArg, functionReducedArg0]));
		}),
	dQ: F2(
		function (getArg, getArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'get'
					}),
				_List_fromArray(
					[getArg, getArg0]));
		}),
	d4: function (hexArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$int]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'hex'
				}),
			_List_fromArray(
				[hexArg]));
	},
	ee: F3(
		function (ifThenArg, ifThenArg0, ifThenArg1) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'ifThen'
					}),
				_List_fromArray(
					[ifThenArg, ifThenArg0, ifThenArg1]));
		}),
	jO: function (intArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$int]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'int'
				}),
			_List_fromArray(
				[intArg]));
	},
	jS: function (justArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil)
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'just'
				}),
			_List_fromArray(
				[justArg]));
	},
	eS: function (listArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'list'
				}),
			_List_fromArray(
				[listArg]));
	},
	e5: function (maybeArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$maybe(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'maybe'
				}),
			_List_fromArray(
				[maybeArg]));
	},
	f3: function (parseArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Result']),
								'Result',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$record(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'declarations',
												$mdgriffith$elm_codegen$Elm$Annotation$list(
													A3(
														$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
														_List_fromArray(
															['Elm']),
														'Declaration',
														_List_Nil)))
											]))
									])))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'parse'
				}),
			_List_fromArray(
				[parseArg]));
	},
	gc: F2(
		function (portIncomingArg, portIncomingArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$list(
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm', 'Annotation']),
											'Annotation',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Declaration',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'portIncoming'
					}),
				_List_fromArray(
					[portIncomingArg, portIncomingArg0]));
		}),
	gd: F2(
		function (portOutgoingArg, portOutgoingArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm', 'Annotation']),
										'Annotation',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Declaration',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'portOutgoing'
					}),
				_List_fromArray(
					[portOutgoingArg, portOutgoingArg0]));
		}),
	gu: function (recordArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A2(
										$mdgriffith$elm_codegen$Elm$Annotation$tuple,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm']),
											'Expression',
											_List_Nil)))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'record'
				}),
			_List_fromArray(
				[recordArg]));
	},
	kO: function (stringArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'string'
				}),
			_List_fromArray(
				[stringArg]));
	},
	hP: function (toStringArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil)
								]),
							$mdgriffith$elm_codegen$Elm$Annotation$string)),
					ei: _List_fromArray(
						['Elm']),
					ao: 'toString'
				}),
			_List_fromArray(
				[toStringArg]));
	},
	hZ: F3(
		function (tripleArg, tripleArg0, tripleArg1) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'triple'
					}),
				_List_fromArray(
					[tripleArg, tripleArg0, tripleArg1]));
		}),
	h_: F2(
		function (tupleArg, tupleArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'tuple'
					}),
				_List_fromArray(
					[tupleArg, tupleArg0]));
		}),
	h7: function (unsafeArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Declaration',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'unsafe'
				}),
			_List_fromArray(
				[unsafeArg]));
	},
	h8: F3(
		function (unwrapArg, unwrapArg0, unwrapArg1) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string),
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'unwrap'
					}),
				_List_fromArray(
					[unwrapArg, unwrapArg0, unwrapArg1]));
		}),
	h9: F2(
		function (unwrapperArg, unwrapperArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string),
										$mdgriffith$elm_codegen$Elm$Annotation$string
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'unwrapper'
					}),
				_List_fromArray(
					[unwrapperArg, unwrapperArg0]));
		}),
	ib: F2(
		function (updateRecordArg, updateRecordArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$list(
										A2(
											$mdgriffith$elm_codegen$Elm$Annotation$tuple,
											$mdgriffith$elm_codegen$Elm$Annotation$string,
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm']),
												'Expression',
												_List_Nil))),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'updateRecord'
					}),
				_List_fromArray(
					[updateRecordArg, updateRecordArg0]));
		}),
	ie: function (valueArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$record(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'importFrom',
											$mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string)),
											_Utils_Tuple2('name', $mdgriffith$elm_codegen$Elm$Annotation$string),
											_Utils_Tuple2(
											'annotation',
											$mdgriffith$elm_codegen$Elm$Annotation$maybe(
												A3(
													$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
													_List_fromArray(
														['Elm', 'Annotation']),
													'Annotation',
													_List_Nil)))
										]))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'value'
				}),
			_List_fromArray(
				[valueArg]));
	},
	bf: function (variantArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Variant',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm']),
					ao: 'variant'
				}),
			_List_fromArray(
				[variantArg]));
	},
	ii: F2(
		function (variantWithArg, variantWithArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$list(
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Elm', 'Annotation']),
											'Annotation',
											_List_Nil))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Variant',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'variantWith'
					}),
				_List_fromArray(
					[variantWithArg, variantWithArg0]));
		}),
	is: F2(
		function (withDocumentationArg, withDocumentationArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Declaration',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Declaration',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'withDocumentation'
					}),
				_List_fromArray(
					[withDocumentationArg, withDocumentationArg0]));
		}),
	iu: F2(
		function (withTypeArg, withTypeArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm', 'Annotation']),
										'Annotation',
										_List_Nil),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil))),
						ei: _List_fromArray(
							['Elm']),
						ao: 'withType'
					}),
				_List_fromArray(
					[withTypeArg, withTypeArg0]));
		})
};
var $author$project$Gen$Elm$nothing = $mdgriffith$elm_codegen$Elm$value(
	{
		bx: $elm$core$Maybe$Just(
			A3(
				$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
				_List_fromArray(
					['Elm']),
				'Expression',
				_List_Nil)),
		ei: _List_fromArray(
			['Elm']),
		ao: 'nothing'
	});
var $author$project$Example$Interactive$Rendered$inputToLiteral = F2(
	function (input, exp) {
		switch (input.$) {
			case 0:
				return $author$project$Gen$Elm$call_.kO(exp);
			case 1:
				return $author$project$Gen$Elm$call_.iS(exp);
			case 2:
				return $author$project$Gen$Elm$call_.jO(exp);
			case 3:
				return $author$project$Gen$Elm$call_.jx(exp);
			default:
				var inner = input.a;
				return $author$project$Gen$Elm$nothing;
		}
	});
var $author$project$Example$Interactive$Rendered$getVal = F3(
	function (nameBase, options, context) {
		var arg = nameBase;
		return {
			jb: _Utils_update(
				context,
				{K: context.K + 1}),
			cQ: A2(
				$author$project$Example$Interactive$Rendered$inputToLiteral,
				options.jN,
				A2($mdgriffith$elm_codegen$Elm$get, arg, context._))
		};
	});
var $author$project$Example$Interactive$Rendered$getValProtected = F3(
	function (nameBase, options, context) {
		var arg = _Utils_ap(
			nameBase,
			$elm$core$String$fromInt(context.K));
		return {
			jb: _Utils_update(
				context,
				{
					K: context.K + 1,
					kM: A2(
						$elm$core$List$cons,
						A2($author$project$Interactive$field, arg, options),
						context.kM)
				}),
			cQ: A2($mdgriffith$elm_codegen$Elm$get, arg, context._)
		};
	});
var $author$project$Gen$Elm$Op$pipe = F2(
	function (pipeArg, pipeArg0) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil),
									A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Elm']),
									'Expression',
									_List_Nil)
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil))),
					ei: _List_fromArray(
						['Elm', 'Op']),
					ao: 'pipe'
				}),
			_List_fromArray(
				[pipeArg, pipeArg0]));
	});
var $author$project$Gen$Elm$record = function (recordArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								$mdgriffith$elm_codegen$Elm$Annotation$list(
								A2(
									$mdgriffith$elm_codegen$Elm$Annotation$tuple,
									$mdgriffith$elm_codegen$Elm$Annotation$string,
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Elm']),
										'Expression',
										_List_Nil)))
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Elm']),
							'Expression',
							_List_Nil))),
				ei: _List_fromArray(
					['Elm']),
				ao: 'record'
			}),
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Elm$list(recordArg)
			]));
};
var $author$project$Gen$Elm$value = function (valueArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								$mdgriffith$elm_codegen$Elm$Annotation$record(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'importFrom',
										$mdgriffith$elm_codegen$Elm$Annotation$list($mdgriffith$elm_codegen$Elm$Annotation$string)),
										_Utils_Tuple2('name', $mdgriffith$elm_codegen$Elm$Annotation$string),
										_Utils_Tuple2(
										'annotation',
										$mdgriffith$elm_codegen$Elm$Annotation$maybe(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Elm', 'Annotation']),
												'Annotation',
												_List_Nil)))
									]))
							]),
						A3(
							$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
							_List_fromArray(
								['Elm']),
							'Expression',
							_List_Nil))),
				ei: _List_fromArray(
					['Elm']),
				ao: 'value'
			}),
		_List_fromArray(
			[
				$mdgriffith$elm_codegen$Elm$record(
				_List_fromArray(
					[
						A2(
						$elm$core$Tuple$pair,
						'importFrom',
						$mdgriffith$elm_codegen$Elm$list(
							A2($elm$core$List$map, $mdgriffith$elm_codegen$Elm$string, valueArg.ei))),
						A2(
						$elm$core$Tuple$pair,
						'name',
						$mdgriffith$elm_codegen$Elm$string(valueArg.ao)),
						A2($elm$core$Tuple$pair, 'annotation', valueArg.bx)
					]))
			]));
};
var $author$project$Example$Interactive$Rendered$buildArg = F4(
	function (options, context, namespace, target) {
		_v5$6:
		while (true) {
			_v5$13:
			while (true) {
				switch (target.$) {
					case 0:
						if (target.a === 'msg') {
							return $elm$core$Result$Ok(
								{
									jb: context,
									cQ: $author$project$Gen$Elm$value(
										{bx: $mdgriffith$elm_codegen$Elm$nothing, ei: _List_Nil, ao: 'Log'})
								});
						} else {
							var _var = target.a;
							return $elm$core$Result$Err('I don\'t know how to build a ' + _var);
						}
					case 1:
						var arg = target.a;
						var result = target.b;
						return $elm$core$Result$Err('Nested lambdas');
					case 2:
						if (!target.a.b) {
							return $elm$core$Result$Ok(
								{jb: context, cQ: $mdgriffith$elm_codegen$Elm$unit});
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
										var _v9 = A4($author$project$Example$Interactive$Rendered$buildArg, options, oneBuilt.jb, namespace, two);
										if (!_v9.$) {
											var twoBuilt = _v9.a;
											return $elm$core$Result$Ok(
												{
													jb: twoBuilt.jb,
													cQ: A2($mdgriffith$elm_codegen$Elm$tuple, oneBuilt.cQ, twoBuilt.cQ)
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
											var _v14 = A4($author$project$Example$Interactive$Rendered$buildArg, options, oneBuilt.jb, namespace, two);
											if (!_v14.$) {
												var twoBuilt = _v14.a;
												var _v15 = A4($author$project$Example$Interactive$Rendered$buildArg, options, twoBuilt.jb, namespace, three);
												if (!_v15.$) {
													var threeBuilt = _v15.a;
													return $elm$core$Result$Ok(
														{
															jb: threeBuilt.jb,
															cQ: A3($mdgriffith$elm_codegen$Elm$triple, oneBuilt.cQ, twoBuilt.cQ, threeBuilt.cQ)
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
								case 'String.String':
									return $elm$core$Result$Ok(
										A3(
											$author$project$Example$Interactive$Rendered$getVal,
											namespace,
											{
												C: $mdgriffith$elm_codegen$Elm$string(''),
												jN: $author$project$Interactive$string
											},
											context));
								case 'Basics.Boolean':
									return $elm$core$Result$Ok(
										A3(
											$author$project$Example$Interactive$Rendered$getVal,
											namespace,
											{
												C: $mdgriffith$elm_codegen$Elm$bool(true),
												jN: $author$project$Interactive$bool
											},
											context));
								case 'Basics.Int':
									return $elm$core$Result$Ok(
										A3(
											$author$project$Example$Interactive$Rendered$getVal,
											namespace,
											{
												C: $mdgriffith$elm_codegen$Elm$int(1),
												jN: $author$project$Interactive$int
											},
											context));
								case 'Basics.Float':
									return $elm$core$Result$Ok(
										A3(
											$author$project$Example$Interactive$Rendered$getVal,
											namespace,
											{
												C: $mdgriffith$elm_codegen$Elm$float(1),
												jN: $author$project$Interactive$float
											},
											context));
								default:
									break _v5$13;
							}
						} else {
							if (!target.b.b.b) {
								switch (target.a) {
									case 'Maybe.Maybe':
										var _v16 = target.b;
										var inner = _v16.a;
										var _v17 = A4($author$project$Example$Interactive$Rendered$buildArg, options, context, namespace, inner);
										if (_v17.$ === 1) {
											var err = _v17.a;
											return $elm$core$Result$Err(err);
										} else {
											var innerExample = _v17.a;
											return $elm$core$Result$Ok(
												{
													jb: innerExample.jb,
													cQ: $mdgriffith$elm_codegen$Elm$just(innerExample.cQ)
												});
										}
									case 'List.List':
										var _v18 = target.b;
										var inner = _v18.a;
										var _v19 = A4($author$project$Example$Interactive$Rendered$buildArg, options, context, namespace, inner);
										if (_v19.$ === 1) {
											var err = _v19.a;
											return $elm$core$Result$Err(err);
										} else {
											var innerExample = _v19.a;
											return $elm$core$Result$Ok(
												{
													jb: innerExample.jb,
													cQ: $mdgriffith$elm_codegen$Elm$list(
														_List_fromArray(
															[innerExample.cQ]))
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
						var renderedResult = A3(
							$elm$core$List$foldl,
							F2(
								function (_v29, gathered) {
									var fieldName = _v29.a;
									var fieldType = _v29.b;
									if (gathered.$ === 1) {
										var err = gathered.a;
										return gathered;
									} else {
										var _v31 = gathered.a;
										var currentContext = _v31.a;
										var renderedFields = _v31.b;
										var _v32 = A4($author$project$Example$Interactive$Rendered$buildArg, options, currentContext, fieldName, fieldType);
										if (!_v32.$) {
											var fieldExample = _v32.a;
											return $elm$core$Result$Ok(
												_Utils_Tuple2(
													fieldExample.jb,
													A2(
														$elm$core$List$cons,
														A2(
															$mdgriffith$elm_codegen$Elm$tuple,
															$mdgriffith$elm_codegen$Elm$string(fieldName),
															fieldExample.cQ),
														renderedFields)));
										} else {
											var err = _v32.a;
											return $elm$core$Result$Err(err);
										}
									}
								}),
							$elm$core$Result$Ok(
								_Utils_Tuple2(context, _List_Nil)),
							fields);
						if (!renderedResult.$) {
							var _v28 = renderedResult.a;
							var newContext = _v28.a;
							var rendered = _v28.b;
							return $elm$core$Result$Ok(
								{
									jb: newContext,
									cQ: $author$project$Gen$Elm$record(rendered)
								});
						} else {
							var err = renderedResult.a;
							return $elm$core$Result$Err(err);
						}
				}
			}
			var name = target.a;
			var vars = target.b;
			return A3(
				$elm$core$List$foldl,
				F2(
					function (decl, buildResult) {
						if (!buildResult.$) {
							return buildResult;
						} else {
							if (A2($author$project$Example$Type$isCreatorOf, name, decl.ay)) {
								if (options.ag) {
									var exampleCall = A5(
										$author$project$Example$Interactive$Rendered$buildExampleCall,
										{ag: false},
										context,
										{
											aV: function (_v25) {
												return true;
											},
											t: decl
										},
										decl.ay,
										_List_Nil);
									if (!exampleCall.$) {
										var builtValue = exampleCall.a;
										var _v22 = A3(
											$elm$core$List$foldl,
											F2(
												function (doc, untouched) {
													var ctxt = untouched.a;
													var existingBuilders = untouched.b;
													var _v23 = A2($author$project$Example$Type$getBuilderOf, name, doc);
													if (_v23.$ === 1) {
														return untouched;
													} else {
														var builder = _v23.a;
														var builtBuilderResult = A5(
															$author$project$Example$Interactive$Rendered$buildBuilder,
															{ag: false},
															ctxt,
															builder,
															builder.ay,
															_List_Nil);
														if (builtBuilderResult.$ === 1) {
															return untouched;
														} else {
															var builtBuilder = builtBuilderResult.a;
															var builderSwitch = A3(
																$author$project$Example$Interactive$Rendered$getValProtected,
																'includeBuilder',
																{
																	C: $mdgriffith$elm_codegen$Elm$bool(false),
																	jN: $author$project$Interactive$bool
																},
																builtBuilder.jb);
															return _Utils_Tuple2(
																builderSwitch.jb,
																A2(
																	$elm$core$List$cons,
																	_Utils_Tuple2(builderSwitch.cQ, builtBuilder.cQ),
																	existingBuilders));
														}
													}
												}),
											_Utils_Tuple2(builtValue.jb, _List_Nil),
											context.A.k$);
										var buildersContext = _v22.a;
										var builders = _v22.b;
										return $elm$core$Result$Ok(
											{
												jb: buildersContext,
												cQ: A3($elm$core$List$foldl, $author$project$Example$Interactive$Rendered$applyBuilder, builtValue.cQ, builders)
											});
									} else {
										var err = exampleCall.a;
										return $elm$core$Result$Err(err);
									}
								} else {
									return A5(
										$author$project$Example$Interactive$Rendered$buildExampleCall,
										{ag: false},
										context,
										{
											aV: function (_v26) {
												return true;
											},
											t: decl
										},
										decl.ay,
										_List_Nil);
								}
							} else {
								return buildResult;
							}
						}
					}),
				$elm$core$Result$Err('I don\'t know how to build a ' + name),
				context.A.k$);
		}
		return $elm$core$Result$Err('I don\'t know how to build a tuple with values other than a 0, 2, and three.');
	});
var $author$project$Example$Interactive$Rendered$buildBuilder = F5(
	function (options, context, originalValue, targetType, args) {
		buildBuilder:
		while (true) {
			if (targetType.$ === 1) {
				if (targetType.b.$ === 1) {
					var arg = targetType.a;
					var result = targetType.b;
					var _v4 = A4($author$project$Example$Interactive$Rendered$buildArg, options, context, originalValue.ao, arg);
					if (!_v4.$) {
						var argBuilt = _v4.a;
						var $temp$options = options,
							$temp$context = argBuilt.jb,
							$temp$originalValue = originalValue,
							$temp$targetType = result,
							$temp$args = A2($elm$core$List$cons, argBuilt.cQ, args);
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
					return $elm$core$Result$Ok(
						{
							jb: context,
							cQ: A2(
								$mdgriffith$elm_codegen$Elm$fn,
								_Utils_Tuple2('a', $elm$core$Maybe$Nothing),
								$author$project$Gen$Elm$Op$pipe(
									A2(
										$author$project$Gen$Elm$apply,
										$author$project$Gen$Elm$value(
											{
												bx: $mdgriffith$elm_codegen$Elm$nothing,
												ei: A2($elm$core$String$split, '.', context.A.ao),
												ao: originalValue.ao
											}),
										$elm$core$List$reverse(args))))
						});
				}
			} else {
				return A4($author$project$Example$Interactive$Rendered$buildArg, options, context, originalValue.ao, targetType);
			}
		}
	});
var $author$project$Example$Interactive$Rendered$buildExampleCall = F5(
	function (options, context, bounds, targetType, args) {
		buildExampleCall:
		while (true) {
			if (targetType.$ === 1) {
				var arg = targetType.a;
				var result = targetType.b;
				var _v1 = A4($author$project$Example$Interactive$Rendered$buildArg, options, context, bounds.t.ao, arg);
				if (!_v1.$) {
					var argBuilt = _v1.a;
					if (result.$ === 1) {
						var $temp$options = options,
							$temp$context = argBuilt.jb,
							$temp$bounds = bounds,
							$temp$targetType = result,
							$temp$args = A2($elm$core$List$cons, argBuilt.cQ, args);
						options = $temp$options;
						context = $temp$context;
						bounds = $temp$bounds;
						targetType = $temp$targetType;
						args = $temp$args;
						continue buildExampleCall;
					} else {
						return $elm$core$Result$Ok(
							{
								jb: argBuilt.jb,
								cQ: A2(
									$author$project$Gen$Elm$apply,
									$author$project$Gen$Elm$value(
										{
											bx: $mdgriffith$elm_codegen$Elm$nothing,
											ei: A2($elm$core$String$split, '.', argBuilt.jb.A.ao),
											ao: bounds.t.ao
										}),
									$elm$core$List$reverse(
										A2($elm$core$List$cons, argBuilt.cQ, args)))
							});
					}
				} else {
					var err = _v1.a;
					return $elm$core$Result$Err(err);
				}
			} else {
				return $elm$core$Result$Ok(
					{
						jb: context,
						cQ: $author$project$Gen$Elm$value(
							{
								bx: $mdgriffith$elm_codegen$Elm$nothing,
								ei: A2($elm$core$String$split, '.', context.A.ao),
								ao: bounds.t.ao
							})
					});
			}
		}
	});
var $author$project$Example$Interactive$Rendered$buildHelper = F3(
	function (options, context, _v0) {
		var callstack = _v0;
		var starterCall = options.a5 ? A5(
			$author$project$Example$Interactive$Rendered$buildBuilder,
			{ag: options.ag},
			context,
			callstack.t,
			callstack.t.ay,
			_List_Nil) : A5(
			$author$project$Example$Interactive$Rendered$buildExampleCall,
			{ag: options.ag},
			context,
			{
				aV: function (_v7) {
					return true;
				},
				t: callstack.t
			},
			callstack.t.ay,
			_List_Nil);
		if (!starterCall.$) {
			var call = starterCall.a;
			return A3(
				$elm$core$List$foldl,
				F2(
					function (step, builtResult) {
						if (!builtResult.$) {
							var built = builtResult.a;
							if (step.a8) {
								var _v3 = A3(
									$author$project$Example$Interactive$Rendered$buildHelper,
									_Utils_update(
										options,
										{a5: true}),
									built.jb,
									step.ba);
								if (!_v3.$) {
									var builtStep = _v3.a;
									return $elm$core$Result$Ok(
										{
											jb: builtStep.jb,
											cQ: A2($mdgriffith$elm_codegen$Elm$Op$pipe, builtStep.cQ, built.cQ)
										});
								} else {
									var err = _v3.a;
									return $elm$core$Result$Err(err);
								}
							} else {
								var _v4 = $author$project$Example$Type$getArgs(
									$author$project$Example$CallStack$start(step.ba).ay);
								_v4$2:
								while (true) {
									if (_v4.b) {
										if (!_v4.b.b) {
											var boolVal = A2(
												$mdgriffith$elm_codegen$Elm$get,
												$author$project$Example$CallStack$name(step.ba),
												built.jb._);
											return $elm$core$Result$Ok(
												{
													jb: built.jb,
													cQ: A2(
														$mdgriffith$elm_codegen$Elm$Op$pipe,
														A3(
															$mdgriffith$elm_codegen$Elm$ifThen,
															boolVal,
															A2(
																$mdgriffith$elm_codegen$Elm$fn,
																_Utils_Tuple2('a', $elm$core$Maybe$Nothing),
																$author$project$Gen$Elm$Op$pipe(
																	A2(
																		$author$project$Gen$Elm$apply,
																		$author$project$Gen$Elm$value(
																			{
																				bx: $mdgriffith$elm_codegen$Elm$nothing,
																				ei: A2($elm$core$String$split, '.', context.A.ao),
																				ao: $author$project$Example$CallStack$name(step.ba)
																			}),
																		_List_Nil))),
															$author$project$Example$Interactive$Rendered$genIdentity),
														built.cQ)
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
													var maybeVal = A2(
														$mdgriffith$elm_codegen$Elm$get,
														$author$project$Example$CallStack$name(step.ba),
														built.jb._);
													return $elm$core$Result$Ok(
														{
															jb: built.jb,
															cQ: A2(
																$mdgriffith$elm_codegen$Elm$Op$pipe,
																A2(
																	$mdgriffith$elm_codegen$Elm$Case$maybe,
																	maybeVal,
																	{
																		jS: _Utils_Tuple2(
																			$author$project$Example$CallStack$name(step.ba) + '_option',
																			function (val) {
																				return A2(
																					$mdgriffith$elm_codegen$Elm$fn,
																					_Utils_Tuple2('a', $elm$core$Maybe$Nothing),
																					$author$project$Gen$Elm$Op$pipe(
																						A2(
																							$author$project$Gen$Elm$apply,
																							$author$project$Gen$Elm$value(
																								{
																									bx: $mdgriffith$elm_codegen$Elm$nothing,
																									ei: A2($elm$core$String$split, '.', context.A.ao),
																									ao: $author$project$Example$CallStack$name(step.ba)
																								}),
																							_List_fromArray(
																								[
																									A2($author$project$Example$Interactive$Rendered$inputToLiteral, input.jN, val)
																								]))));
																			}),
																		j8: $author$project$Example$Interactive$Rendered$genIdentity
																	}),
																built.cQ)
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
							var err = builtResult.a;
							return $elm$core$Result$Err(err);
						}
					}),
				$elm$core$Result$Ok(call),
				$elm$core$List$reverse(callstack.aM));
		} else {
			var err = starterCall.a;
			return $elm$core$Result$Err(err);
		}
	});
var $author$project$Example$Interactive$Rendered$initContext = function (modul) {
	return {
		K: 0,
		_: $mdgriffith$elm_codegen$Elm$value(
			{bx: $elm$core$Maybe$Nothing, ei: _List_Nil, ao: 'model'}),
		A: modul,
		kM: _List_Nil
	};
};
var $author$project$Gen$Elm$toString = function (toStringArg) {
	return A2(
		$mdgriffith$elm_codegen$Elm$apply,
		$mdgriffith$elm_codegen$Elm$value(
			{
				bx: $elm$core$Maybe$Just(
					A2(
						$mdgriffith$elm_codegen$Elm$Annotation$function,
						_List_fromArray(
							[
								A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Elm']),
								'Expression',
								_List_Nil)
							]),
						$mdgriffith$elm_codegen$Elm$Annotation$string)),
				ei: _List_fromArray(
					['Elm']),
				ao: 'toString'
			}),
		_List_fromArray(
			[toStringArg]));
};
var $author$project$Example$Interactive$Rendered$build = F2(
	function (mod, callstack) {
		return A2(
			$elm$core$Result$map,
			function (ok) {
				return _Utils_update(
					ok,
					{
						cQ: $author$project$Gen$Elm$toString(ok.cQ)
					});
			},
			A3(
				$author$project$Example$Interactive$Rendered$buildHelper,
				{ag: true, a5: false},
				$author$project$Example$Interactive$Rendered$initContext(mod),
				callstack));
	});
var $author$project$Example$CallStack$CallStack = $elm$core$Basics$identity;
var $author$project$Example$Type$getResultType = function (tipe) {
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
var $author$project$Example$Type$listMatches = F2(
	function (ones, twos) {
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
							var $temp$ones = oneRemain,
								$temp$twos = twoRemain;
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
var $author$project$Example$Type$matches = F2(
	function (one, two) {
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
var $author$project$Example$Type$isBuilderOf = F2(
	function (desiredResult, possibleBuilder) {
		isBuilderOf:
		while (true) {
			if (possibleBuilder.$ === 1) {
				var arg = possibleBuilder.a;
				var result = possibleBuilder.b;
				if (A2($author$project$Example$Type$matches, desiredResult, arg) && A2($author$project$Example$Type$matches, desiredResult, result)) {
					return true;
				} else {
					var $temp$desiredResult = desiredResult,
						$temp$possibleBuilder = result;
					desiredResult = $temp$desiredResult;
					possibleBuilder = $temp$possibleBuilder;
					continue isBuilderOf;
				}
			} else {
				return false;
			}
		}
	});
var $author$project$Example$CallStack$matchesResultType = F2(
	function (one, two) {
		return A2(
			$author$project$Example$Type$matches,
			$author$project$Example$Type$getResultType(one.ay),
			$author$project$Example$Type$getResultType(two.ay));
	});
var $author$project$Example$CallStack$mergeCallStacks = F2(
	function (maybeOne, maybeTwo) {
		var _v0 = _Utils_Tuple2(maybeOne, maybeTwo);
		if (!_v0.a.$) {
			if (!_v0.b.$) {
				var one = _v0.a.a;
				var two = _v0.b.a;
				return $elm$core$Maybe$Just(
					_Utils_ap(one, two));
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
var $author$project$Example$CallStack$singleCall = function (val) {
	return {
		ap: $author$project$Example$Type$getResultType(val.ay),
		t: val,
		aM: _List_Nil
	};
};
var $author$project$Example$CallStack$find = F3(
	function (inScope, built, targeting) {
		var resultType = $author$project$Example$Type$getResultType(targeting.t.ay);
		return targeting.aV(resultType) ? $elm$core$Maybe$Just(
			_List_fromArray(
				[
					{
					ap: resultType,
					t: targeting.t,
					aM: A2(
						$elm$core$List$filterMap,
						function (val) {
							return A2($author$project$Example$Type$isBuilderOf, resultType, val.ay) ? $elm$core$Maybe$Just(
								{
									a8: false,
									ba: $author$project$Example$CallStack$singleCall(val)
								}) : $elm$core$Maybe$Nothing;
						},
						inScope)
				}
				])) : A3(
			$elm$core$List$foldl,
			F2(
				function (val, gathered) {
					if (A2($author$project$Example$CallStack$matchesResultType, val, targeting.t)) {
						return gathered;
					} else {
						if (A2(
							$elm$core$List$any,
							$author$project$Example$CallStack$matchesResultType(val),
							built)) {
							return gathered;
						} else {
							var maybeSubCallStacks = A3(
								$author$project$Example$CallStack$find,
								inScope,
								A2($elm$core$List$cons, targeting.t, built),
								{aV: targeting.aV, t: val});
							if (maybeSubCallStacks.$ === 1) {
								return gathered;
							} else {
								var subCallStacks = maybeSubCallStacks.a;
								return A2(
									$author$project$Example$CallStack$mergeCallStacks,
									gathered,
									$elm$core$Maybe$Just(
										A2(
											$elm$core$List$map,
											function (subCall) {
												var subCallDetails = subCall;
												var optionalBuilders = A2(
													$elm$core$List$filterMap,
													function (optionalVal) {
														return A2($author$project$Example$Type$isBuilderOf, resultType, optionalVal.ay) ? $elm$core$Maybe$Just(
															{
																a8: false,
																ba: $author$project$Example$CallStack$singleCall(optionalVal)
															}) : $elm$core$Maybe$Nothing;
													},
													inScope);
												return {
													ap: subCallDetails.ap,
													t: targeting.t,
													aM: A2(
														$elm$core$List$cons,
														{
															a8: true,
															ba: $author$project$Example$CallStack$singleCall(subCallDetails.t)
														},
														_Utils_ap(optionalBuilders, subCallDetails.aM))
												};
											},
											subCallStacks)));
							}
						}
					}
				}),
			$elm$core$Maybe$Nothing,
			inScope);
	});
var $author$project$Example$CallStack$getResultType = function (_v0) {
	var call = _v0;
	return call.ap;
};
var $author$project$Example$Interactive$runnerEnd = F2(
	function (runners, target) {
		runnerEnd:
		while (true) {
			if (!runners.b) {
				return false;
			} else {
				var runner = runners.a;
				var remain = runners.b;
				if (runner.aR(target)) {
					return true;
				} else {
					var $temp$runners = remain,
						$temp$target = target;
					runners = $temp$runners;
					target = $temp$target;
					continue runnerEnd;
				}
			}
		}
	});
var $author$project$Example$Interactive$buildExampleCallStack = F2(
	function (mod, bounds) {
		var _v0 = A3(
			$author$project$Example$CallStack$find,
			mod.k$,
			_List_Nil,
			{
				aV: $author$project$Example$Interactive$runnerEnd(bounds.gR),
				t: bounds.t
			});
		if (_v0.$ === 1) {
			return $elm$core$Result$Err('No way to build desired type');
		} else {
			if (!_v0.a.b) {
				return $elm$core$Result$Err('No way to build desired type');
			} else {
				var _v1 = _v0.a;
				var callStack = _v1.a;
				var calls = _v1.b;
				var renderedResult = A2($author$project$Example$Interactive$Rendered$build, mod, callStack);
				var exampleResult = A2($author$project$Example$Interactive$Build$build, mod, callStack);
				var _v2 = _Utils_Tuple2(renderedResult, exampleResult);
				if ((!_v2.a.$) && (!_v2.b.$)) {
					var rendered = _v2.a.a;
					var example = _v2.b.a;
					return $elm$core$Result$Ok(
						{
							c1: rendered,
							a7: example,
							gB: $author$project$Example$CallStack$getResultType(callStack)
						});
				} else {
					return $elm$core$Result$Err('Something terribly terribly wrong happened');
				}
			}
		}
	});
var $author$project$Gen$Ui$call_ = {
	bZ: function (cardArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_Nil,
										'Html',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
											])))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_Nil,
								'Html',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									])))),
					ei: _List_fromArray(
						['Ui']),
					ao: 'card'
				}),
			_List_fromArray(
				[cardArg]));
	},
	i5: F2(
		function (codeArg, codeArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$string, $mdgriffith$elm_codegen$Elm$Annotation$string]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Element']),
									'Element',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
										])))),
						ei: _List_fromArray(
							['Ui']),
						ao: 'code'
					}),
				_List_fromArray(
					[codeArg, codeArg0]));
		}),
	eO: function (lightCodeArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Element',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
									])))),
					ei: _List_fromArray(
						['Ui']),
					ao: 'lightCode'
				}),
			_List_fromArray(
				[lightCodeArg]));
	}
};
var $author$project$Gen$Element$centerY = $mdgriffith$elm_codegen$Elm$value(
	{
		bx: $elm$core$Maybe$Just(
			A3(
				$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
				_List_fromArray(
					['Element']),
				'Attribute',
				_List_fromArray(
					[
						$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
					]))),
		ei: _List_fromArray(
			['Element']),
		ao: 'centerY'
	});
var $author$project$Example$Interactive$getRunner = F2(
	function (runners, tipe) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (run, maybe) {
					if (maybe.$ === 1) {
						return run.aR(tipe) ? $elm$core$Maybe$Just(run) : $elm$core$Maybe$Nothing;
					} else {
						return maybe;
					}
				}),
			$elm$core$Maybe$Nothing,
			runners);
	});
var $author$project$Gen$Element$minimum = F2(
	function (minimumArg, minimumArg0) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$int,
									A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Element']),
									'Length',
									_List_Nil)
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Element']),
								'Length',
								_List_Nil))),
					ei: _List_fromArray(
						['Element']),
					ao: 'minimum'
				}),
			_List_fromArray(
				[
					$mdgriffith$elm_codegen$Elm$int(minimumArg),
					minimumArg0
				]));
	});
var $author$project$Gen$Element$shrink = $mdgriffith$elm_codegen$Elm$value(
	{
		bx: $elm$core$Maybe$Just(
			A3(
				$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
				_List_fromArray(
					['Element']),
				'Length',
				_List_Nil)),
		ei: _List_fromArray(
			['Element']),
		ao: 'shrink'
	});
var $author$project$Gen$Ui$Input$call_ = {
	iS: F3(
		function (boolArg, boolArg0, boolArg1) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[$mdgriffith$elm_codegen$Elm$Annotation$bool]),
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')),
										$mdgriffith$elm_codegen$Elm$Annotation$bool
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Element']),
									'Element',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
										])))),
						ei: _List_fromArray(
							['Ui', 'Input']),
						ao: 'bool'
					}),
				_List_fromArray(
					[boolArg, boolArg0, boolArg1]));
		}),
	jO: F3(
		function (intArg, intArg0, intArg1) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[$mdgriffith$elm_codegen$Elm$Annotation$int]),
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')),
										$mdgriffith$elm_codegen$Elm$Annotation$int
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Element']),
									'Element',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
										])))),
						ei: _List_fromArray(
							['Ui', 'Input']),
						ao: 'int'
					}),
				_List_fromArray(
					[intArg, intArg0, intArg1]));
		}),
	jZ: F3(
		function (maybeBoolArg, maybeBoolArg0, maybeBoolArg1) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_Nil,
												'Maybe',
												_List_fromArray(
													[$mdgriffith$elm_codegen$Elm$Annotation$bool]))
											]),
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_Nil,
										'Maybe',
										_List_fromArray(
											[$mdgriffith$elm_codegen$Elm$Annotation$bool]))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Element']),
									'Element',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
										])))),
						ei: _List_fromArray(
							['Ui', 'Input']),
						ao: 'maybeBool'
					}),
				_List_fromArray(
					[maybeBoolArg, maybeBoolArg0, maybeBoolArg1]));
		}),
	j_: F3(
		function (maybeStringArg, maybeStringArg0, maybeStringArg1) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_Nil,
												'Maybe',
												_List_fromArray(
													[$mdgriffith$elm_codegen$Elm$Annotation$string]))
											]),
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_Nil,
										'Maybe',
										_List_fromArray(
											[$mdgriffith$elm_codegen$Elm$Annotation$string]))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Element']),
									'Element',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
										])))),
						ei: _List_fromArray(
							['Ui', 'Input']),
						ao: 'maybeString'
					}),
				_List_fromArray(
					[maybeStringArg, maybeStringArg0, maybeStringArg1]));
		}),
	kO: F3(
		function (stringArg, stringArg0, stringArg1) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[$mdgriffith$elm_codegen$Elm$Annotation$string]),
										$mdgriffith$elm_codegen$Elm$Annotation$var('msg')),
										$mdgriffith$elm_codegen$Elm$Annotation$string
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Element']),
									'Element',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
										])))),
						ei: _List_fromArray(
							['Ui', 'Input']),
						ao: 'string'
					}),
				_List_fromArray(
					[stringArg, stringArg0, stringArg1]));
		})
};
var $author$project$Interactive$details = function (_v0) {
	var name = _v0.a;
	var opts = _v0.b;
	return {
		jN: opts.jN,
		Z: name,
		am: A2($elm$core$String$startsWith, 'with', name) ? A3($elm$core$String$replace, 'with', '', name) : name,
		kb: opts.jN
	};
};
var $author$project$Example$Interactive$viewFieldInput = F2(
	function (opts, field) {
		var details = $author$project$Interactive$details(field);
		var updateValue = A2(
			$mdgriffith$elm_codegen$Elm$fn,
			_Utils_Tuple2('new', $elm$core$Maybe$Nothing),
			function (_new) {
				return A2(
					$mdgriffith$elm_codegen$Elm$apply,
					opts.kb,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_codegen$Elm$updateRecord,
							_List_fromArray(
								[
									_Utils_Tuple2(details.Z, _new)
								]),
							opts._)
						]));
			});
		var _v0 = details.jN;
		switch (_v0.$) {
			case 0:
				return A3(
					$author$project$Gen$Ui$Input$call_.kO,
					$mdgriffith$elm_codegen$Elm$string(details.am),
					updateValue,
					A2($mdgriffith$elm_codegen$Elm$get, details.Z, opts._));
			case 1:
				return A3(
					$author$project$Gen$Ui$Input$call_.iS,
					$mdgriffith$elm_codegen$Elm$string(details.am),
					updateValue,
					A2($mdgriffith$elm_codegen$Elm$get, details.Z, opts._));
			case 2:
				return A3(
					$author$project$Gen$Ui$Input$call_.jO,
					$mdgriffith$elm_codegen$Elm$string(details.am),
					updateValue,
					A2($mdgriffith$elm_codegen$Elm$get, details.Z, opts._));
			case 3:
				return $author$project$Gen$Element$text('Float');
			default:
				switch (_v0.a.$) {
					case 0:
						var _v1 = _v0.a;
						return A3(
							$author$project$Gen$Ui$Input$call_.j_,
							$mdgriffith$elm_codegen$Elm$string(details.am),
							updateValue,
							A2($mdgriffith$elm_codegen$Elm$get, details.Z, opts._));
					case 1:
						var _v2 = _v0.a;
						return A3(
							$author$project$Gen$Ui$Input$call_.jZ,
							$mdgriffith$elm_codegen$Elm$string(details.am),
							updateValue,
							A2($mdgriffith$elm_codegen$Elm$get, details.Z, opts._));
					default:
						return $author$project$Gen$Element$text('Float');
				}
		}
	});
var $author$project$Example$Interactive$viewInput = F2(
	function (viewOptions, fields) {
		return A2(
			$author$project$Gen$Element$column,
			_List_fromArray(
				[
					$author$project$Gen$Element$width($author$project$Gen$Element$fill),
					$author$project$Gen$Element$spacing(16)
				]),
			A2(
				$elm$core$List$map,
				$author$project$Example$Interactive$viewFieldInput(viewOptions),
				$elm$core$List$reverse(fields)));
	});
var $author$project$Example$Interactive$build = F2(
	function (modul, targeting) {
		var _v0 = A2($author$project$Example$Interactive$buildExampleCallStack, modul, targeting);
		if (!_v0.$) {
			var example = _v0.a;
			var _v1 = A2(
				$author$project$Example$Interactive$getRunner,
				targeting.gR,
				$author$project$Example$Type$getResultType(example.gB));
			if (_v1.$ === 1) {
				return $elm$core$Result$Err('No Runner!  Huh, this shouldn\'t happen');
			} else {
				var runner = _v1.a;
				var fields = _Utils_ap(runner.aX, example.a7.jb.kM);
				return $elm$core$Result$Ok(
					{
						aX: fields,
						ao: targeting.t.ao,
						bg: function (opts) {
							return A2(
								$author$project$Gen$Element$column,
								_List_fromArray(
									[
										$author$project$Gen$Element$width($author$project$Gen$Element$fill),
										$author$project$Gen$Element$height($author$project$Gen$Element$fill)
									]),
								_List_fromArray(
									[
										A3(
										$mdgriffith$elm_codegen$Elm$ifThen,
										opts.i6,
										A2(
											$author$project$Gen$Element$el,
											_List_fromArray(
												[
													$author$project$Gen$Element$width($author$project$Gen$Element$fill),
													$author$project$Gen$Element$Font$color(
													A3($author$project$Gen$Element$rgb, 0, 0, 0)),
													$author$project$Gen$Element$Background$color(
													A3($author$project$Gen$Element$rgb, 1, 1, 1))
												]),
											A2(runner.bg, opts, example.a7.cQ)),
										A2(
											$author$project$Gen$Element$el,
											_List_fromArray(
												[
													$author$project$Gen$Element$padding(32),
													$author$project$Gen$Element$height(
													A2($author$project$Gen$Element$minimum, 200, $author$project$Gen$Element$shrink))
												]),
											A2(
												$author$project$Gen$Element$el,
												_List_fromArray(
													[$author$project$Gen$Element$centerY]),
												A2(
													$author$project$Gen$Ui$call_.i5,
													$mdgriffith$elm_codegen$Elm$string(''),
													example.c1.cQ)))),
										A2(
										$author$project$Gen$Element$el,
										_List_fromArray(
											[
												$author$project$Gen$Element$width($author$project$Gen$Element$fill),
												$author$project$Gen$Element$padding(32)
											]),
										A2($author$project$Example$Interactive$viewInput, opts, fields))
									]));
						}
					});
			}
		} else {
			var err = _v0.a;
			return $elm$core$Result$Err(err);
		}
	});
var $author$project$Gen$Element$centerX = $mdgriffith$elm_codegen$Elm$value(
	{
		bx: $elm$core$Maybe$Just(
			A3(
				$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
				_List_fromArray(
					['Element']),
				'Attribute',
				_List_fromArray(
					[
						$mdgriffith$elm_codegen$Elm$Annotation$var('msg')
					]))),
		ei: _List_fromArray(
			['Element']),
		ao: 'centerX'
	});
var $author$project$Exemplar$element = {
	aR: function (tipe) {
		if ((tipe.$ === 3) && (tipe.a === 'Element.Element')) {
			return true;
		} else {
			return false;
		}
	},
	aX: _List_Nil,
	bg: F2(
		function (_v1, val) {
			var model = _v1._;
			var onChange = _v1.kb;
			return A2(
				$author$project$Gen$Element$el,
				_List_fromArray(
					[
						$author$project$Gen$Element$padding(32),
						$author$project$Gen$Element$height(
						A2($author$project$Gen$Element$minimum, 200, $author$project$Gen$Element$shrink))
					]),
				A2(
					$author$project$Gen$Element$el,
					_List_fromArray(
						[$author$project$Gen$Element$centerY, $author$project$Gen$Element$centerX]),
					val));
		})
};
var $author$project$Example$Build$getValueNamed = F2(
	function (name, values) {
		getValueNamed:
		while (true) {
			if (!values.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var top = values.a;
				var remain = values.b;
				if (_Utils_eq(top.ao, name)) {
					return $elm$core$Maybe$Just(top);
				} else {
					var $temp$name = name,
						$temp$values = remain;
					name = $temp$name;
					values = $temp$values;
					continue getValueNamed;
				}
			}
		}
	});
var $author$project$Example$Type$isBuilder = function (possibleBuilder) {
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
var $author$project$Example$Type$primitiveNames = _List_fromArray(
	['String.String', 'Basics.Bool', 'Basics.Int', 'Basics.Float']);
var $author$project$Example$Type$primitiveSingleContainers = _List_fromArray(
	['List.List', 'Maybe.Maybe']);
var $author$project$Example$Type$isPrimitive = function (tipe) {
	isPrimitive:
	while (true) {
		switch (tipe.$) {
			case 0:
				if (tipe.a === 'msg') {
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
				return A2(
					$elm$core$List$all,
					A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $author$project$Example$Type$isPrimitive),
					fields);
		}
	}
};
var $author$project$Example$Type$isStartingPointHelper = function (tipe) {
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
var $author$project$Example$Type$isStartingPoint = function (tipe) {
	return $author$project$Example$Type$isBuilder(tipe) ? false : $author$project$Example$Type$isStartingPointHelper(tipe);
};
var $author$project$Gen$Parser$call_ = {
	bw: F2(
		function (andThenArg, andThenArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('a')
											]),
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Parser']),
											'Parser',
											_List_fromArray(
												[
													$mdgriffith$elm_codegen$Elm$Annotation$var('b')
												]))),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Parser']),
										'Parser',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('a')
											]))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Parser']),
									'Parser',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('b')
										])))),
						ei: _List_fromArray(
							['Parser']),
						ao: 'andThen'
					}),
				_List_fromArray(
					[andThenArg, andThenArg0]));
		}),
	bJ: function (backtrackableArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Parser']),
									'Parser',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('a')
										]))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('a')
									])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'backtrackable'
				}),
			_List_fromArray(
				[backtrackableArg]));
	},
	b1: function (chompIfArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_codegen$Elm$Annotation$function,
									_List_fromArray(
										[$mdgriffith$elm_codegen$Elm$Annotation$char]),
									$mdgriffith$elm_codegen$Elm$Annotation$bool)
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'chompIf'
				}),
			_List_fromArray(
				[chompIfArg]));
	},
	b2: function (chompUntilArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'chompUntil'
				}),
			_List_fromArray(
				[chompUntilArg]));
	},
	b3: function (chompUntilEndOrArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'chompUntilEndOr'
				}),
			_List_fromArray(
				[chompUntilEndOrArg]));
	},
	b4: function (chompWhileArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_codegen$Elm$Annotation$function,
									_List_fromArray(
										[$mdgriffith$elm_codegen$Elm$Annotation$char]),
									$mdgriffith$elm_codegen$Elm$Annotation$bool)
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'chompWhile'
				}),
			_List_fromArray(
				[chompWhileArg]));
	},
	ce: function (commitArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('a')
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('a')
									])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'commit'
				}),
			_List_fromArray(
				[commitArg]));
	},
	cu: function (deadEndsToStringArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Parser']),
										'DeadEnd',
										_List_Nil))
								]),
							$mdgriffith$elm_codegen$Elm$Annotation$string)),
					ei: _List_fromArray(
						['Parser']),
					ao: 'deadEndsToString'
				}),
			_List_fromArray(
				[deadEndsToStringArg]));
	},
	dR: function (getChompedStringArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Parser']),
									'Parser',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('a')
										]))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$string])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'getChompedString'
				}),
			_List_fromArray(
				[getChompedStringArg]));
	},
	eD: function (keywordArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'keyword'
				}),
			_List_fromArray(
				[keywordArg]));
	},
	eJ: function (lazyArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_codegen$Elm$Annotation$function,
									_List_fromArray(
										[$mdgriffith$elm_codegen$Elm$Annotation$unit]),
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Parser']),
										'Parser',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('a')
											])))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('a')
									])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'lazy'
				}),
			_List_fromArray(
				[lazyArg]));
	},
	eP: function (lineCommentArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'lineComment'
				}),
			_List_fromArray(
				[lineCommentArg]));
	},
	eT: F2(
		function (loopArg, loopArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('state'),
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('state')
											]),
										A3(
											$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
											_List_fromArray(
												['Parser']),
											'Parser',
											_List_fromArray(
												[
													A3(
													$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
													_List_fromArray(
														['Parser']),
													'Step',
													_List_fromArray(
														[
															$mdgriffith$elm_codegen$Elm$Annotation$var('state'),
															$mdgriffith$elm_codegen$Elm$Annotation$var('a')
														]))
												])))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Parser']),
									'Parser',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('a')
										])))),
						ei: _List_fromArray(
							['Parser']),
						ao: 'loop'
					}),
				_List_fromArray(
					[loopArg, loopArg0]));
		}),
	eZ: F2(
		function (mapArg, mapArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('a')
											]),
										$mdgriffith$elm_codegen$Elm$Annotation$var('b')),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Parser']),
										'Parser',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('a')
											]))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Parser']),
									'Parser',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('b')
										])))),
						ei: _List_fromArray(
							['Parser']),
						ao: 'map'
					}),
				_List_fromArray(
					[mapArg, mapArg0]));
		}),
	e$: F2(
		function (mapChompedStringArg, mapChompedStringArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_codegen$Elm$Annotation$function,
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$string,
												$mdgriffith$elm_codegen$Elm$Annotation$var('a')
											]),
										$mdgriffith$elm_codegen$Elm$Annotation$var('b')),
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Parser']),
										'Parser',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('a')
											]))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Parser']),
									'Parser',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('b')
										])))),
						ei: _List_fromArray(
							['Parser']),
						ao: 'mapChompedString'
					}),
				_List_fromArray(
					[mapChompedStringArg, mapChompedStringArg0]));
		}),
	fq: F3(
		function (multiCommentArg, multiCommentArg0, multiCommentArg1) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										$mdgriffith$elm_codegen$Elm$Annotation$string,
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Parser']),
										'Nestable',
										_List_Nil)
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Parser']),
									'Parser',
									_List_fromArray(
										[$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
						ei: _List_fromArray(
							['Parser']),
						ao: 'multiComment'
					}),
				_List_fromArray(
					[multiCommentArg, multiCommentArg0, multiCommentArg1]));
		}),
	fA: function (numberArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$record(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'int',
											$mdgriffith$elm_codegen$Elm$Annotation$maybe(
												A2(
													$mdgriffith$elm_codegen$Elm$Annotation$function,
													_List_fromArray(
														[$mdgriffith$elm_codegen$Elm$Annotation$int]),
													$mdgriffith$elm_codegen$Elm$Annotation$var('a')))),
											_Utils_Tuple2(
											'hex',
											$mdgriffith$elm_codegen$Elm$Annotation$maybe(
												A2(
													$mdgriffith$elm_codegen$Elm$Annotation$function,
													_List_fromArray(
														[$mdgriffith$elm_codegen$Elm$Annotation$int]),
													$mdgriffith$elm_codegen$Elm$Annotation$var('a')))),
											_Utils_Tuple2(
											'octal',
											$mdgriffith$elm_codegen$Elm$Annotation$maybe(
												A2(
													$mdgriffith$elm_codegen$Elm$Annotation$function,
													_List_fromArray(
														[$mdgriffith$elm_codegen$Elm$Annotation$int]),
													$mdgriffith$elm_codegen$Elm$Annotation$var('a')))),
											_Utils_Tuple2(
											'binary',
											$mdgriffith$elm_codegen$Elm$Annotation$maybe(
												A2(
													$mdgriffith$elm_codegen$Elm$Annotation$function,
													_List_fromArray(
														[$mdgriffith$elm_codegen$Elm$Annotation$int]),
													$mdgriffith$elm_codegen$Elm$Annotation$var('a')))),
											_Utils_Tuple2(
											'float',
											$mdgriffith$elm_codegen$Elm$Annotation$maybe(
												A2(
													$mdgriffith$elm_codegen$Elm$Annotation$function,
													_List_fromArray(
														[$mdgriffith$elm_codegen$Elm$Annotation$float]),
													$mdgriffith$elm_codegen$Elm$Annotation$var('a'))))
										]))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('a')
									])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'number'
				}),
			_List_fromArray(
				[numberArg]));
	},
	fP: function (oneOfArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$list(
									A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Parser']),
										'Parser',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('a')
											])))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('a')
									])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'oneOf'
				}),
			_List_fromArray(
				[oneOfArg]));
	},
	Q: function (problemArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('a')
									])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'problem'
				}),
			_List_fromArray(
				[problemArg]));
	},
	ku: F2(
		function (runArg, runArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Parser']),
										'Parser',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('a')
											])),
										$mdgriffith$elm_codegen$Elm$Annotation$string
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Result']),
									'Result',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$list(
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Parser']),
												'DeadEnd',
												_List_Nil)),
											$mdgriffith$elm_codegen$Elm$Annotation$var('a')
										])))),
						ei: _List_fromArray(
							['Parser']),
						ao: 'run'
					}),
				_List_fromArray(
					[runArg, runArg0]));
		}),
	g$: function (sequenceArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$record(
									_List_fromArray(
										[
											_Utils_Tuple2('start', $mdgriffith$elm_codegen$Elm$Annotation$string),
											_Utils_Tuple2('separator', $mdgriffith$elm_codegen$Elm$Annotation$string),
											_Utils_Tuple2('end', $mdgriffith$elm_codegen$Elm$Annotation$string),
											_Utils_Tuple2(
											'spaces',
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Parser']),
												'Parser',
												_List_fromArray(
													[$mdgriffith$elm_codegen$Elm$Annotation$unit]))),
											_Utils_Tuple2(
											'item',
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Parser']),
												'Parser',
												_List_fromArray(
													[
														$mdgriffith$elm_codegen$Elm$Annotation$var('a')
													]))),
											_Utils_Tuple2(
											'trailing',
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Parser']),
												'Trailing',
												_List_Nil))
										]))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$list(
										$mdgriffith$elm_codegen$Elm$Annotation$var('a'))
									])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'sequence'
				}),
			_List_fromArray(
				[sequenceArg]));
	},
	hk: function (succeedArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$var('a')
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$var('a')
									])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'succeed'
				}),
			_List_fromArray(
				[succeedArg]));
	},
	ho: function (symbolArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'symbol'
				}),
			_List_fromArray(
				[symbolArg]));
	},
	hS: function (tokenArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[$mdgriffith$elm_codegen$Elm$Annotation$string]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$unit])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'token'
				}),
			_List_fromArray(
				[tokenArg]));
	},
	ig: function (variableArg) {
		return A2(
			$mdgriffith$elm_codegen$Elm$apply,
			$mdgriffith$elm_codegen$Elm$value(
				{
					bx: $elm$core$Maybe$Just(
						A2(
							$mdgriffith$elm_codegen$Elm$Annotation$function,
							_List_fromArray(
								[
									$mdgriffith$elm_codegen$Elm$Annotation$record(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'start',
											A2(
												$mdgriffith$elm_codegen$Elm$Annotation$function,
												_List_fromArray(
													[$mdgriffith$elm_codegen$Elm$Annotation$char]),
												$mdgriffith$elm_codegen$Elm$Annotation$bool)),
											_Utils_Tuple2(
											'inner',
											A2(
												$mdgriffith$elm_codegen$Elm$Annotation$function,
												_List_fromArray(
													[$mdgriffith$elm_codegen$Elm$Annotation$char]),
												$mdgriffith$elm_codegen$Elm$Annotation$bool)),
											_Utils_Tuple2(
											'reserved',
											A3(
												$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
												_List_fromArray(
													['Set']),
												'Set',
												_List_fromArray(
													[$mdgriffith$elm_codegen$Elm$Annotation$string])))
										]))
								]),
							A3(
								$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
								_List_fromArray(
									['Parser']),
								'Parser',
								_List_fromArray(
									[$mdgriffith$elm_codegen$Elm$Annotation$string])))),
					ei: _List_fromArray(
						['Parser']),
					ao: 'variable'
				}),
			_List_fromArray(
				[variableArg]));
	},
	it: F2(
		function (withIndentArg, withIndentArg0) {
			return A2(
				$mdgriffith$elm_codegen$Elm$apply,
				$mdgriffith$elm_codegen$Elm$value(
					{
						bx: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_codegen$Elm$Annotation$function,
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Elm$Annotation$int,
										A3(
										$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
										_List_fromArray(
											['Parser']),
										'Parser',
										_List_fromArray(
											[
												$mdgriffith$elm_codegen$Elm$Annotation$var('a')
											]))
									]),
								A3(
									$mdgriffith$elm_codegen$Elm$Annotation$namedWith,
									_List_fromArray(
										['Parser']),
									'Parser',
									_List_fromArray(
										[
											$mdgriffith$elm_codegen$Elm$Annotation$var('a')
										])))),
						ei: _List_fromArray(
							['Parser']),
						ao: 'withIndent'
					}),
				_List_fromArray(
					[withIndentArg, withIndentArg0]));
		})
};
var $mdgriffith$elm_codegen$Elm$Case$result = F2(
	function (mainExpression, branches) {
		return function (index) {
			var _v0 = A4(
				$mdgriffith$elm_codegen$Elm$Case$captureCase,
				mainExpression,
				_List_Nil,
				$mdgriffith$elm_codegen$Internal$Index$dive(index),
				_List_fromArray(
					[
						function (branchIndex) {
						var _v1 = branches.ka;
						var okNameStr = _v1.a;
						var toOk = _v1.b;
						var _v2 = A2($mdgriffith$elm_codegen$Internal$Compiler$var, branchIndex, okNameStr);
						var okIndex = _v2.a;
						var okName = _v2.b;
						var okExp = _v2.c;
						return _Utils_Tuple3(
							okIndex,
							A2(
								$stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern,
								{a1: _List_Nil, ao: 'Ok'},
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Internal$Compiler$nodify(
										$stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(okName))
									])),
							toOk(okExp));
					},
						function (branchIndex) {
						var _v3 = branches.jn;
						var errNameStr = _v3.a;
						var toErr = _v3.b;
						var _v4 = A2($mdgriffith$elm_codegen$Internal$Compiler$var, branchIndex, errNameStr);
						var errIndex = _v4.a;
						var errName = _v4.b;
						var errExp = _v4.c;
						return _Utils_Tuple3(
							errIndex,
							A2(
								$stil4m$elm_syntax$Elm$Syntax$Pattern$NamedPattern,
								{a1: _List_Nil, ao: 'Err'},
								_List_fromArray(
									[
										$mdgriffith$elm_codegen$Internal$Compiler$nodify(
										$stil4m$elm_syntax$Elm$Syntax$Pattern$VarPattern(errName))
									])),
							toErr(errExp));
					}
					]));
			var expr = _v0.a;
			var gathered = _v0.b;
			return {
				bx: function () {
					var _v5 = gathered.bx;
					if (_v5.$ === 1) {
						return $elm$core$Result$Err(
							_List_fromArray(
								[$mdgriffith$elm_codegen$Internal$Compiler$EmptyCaseStatement]));
					} else {
						var ann = _v5.a;
						return ann;
					}
				}(),
				jr: $stil4m$elm_syntax$Elm$Syntax$Expression$CaseExpression(
					{
						j: $elm$core$List$reverse(gathered.j),
						jr: $mdgriffith$elm_codegen$Internal$Compiler$nodify(expr.jr)
					}),
				a: _Utils_ap(expr.a, gathered.a)
			};
		};
	});
var $author$project$Exemplar$parser = {
	aR: function (tipe) {
		if ((tipe.$ === 3) && (tipe.a === 'Parser.Parser')) {
			return true;
		} else {
			return false;
		}
	},
	aX: _List_fromArray(
		[
			A2(
			$author$project$Interactive$field,
			'Source',
			{
				C: $mdgriffith$elm_codegen$Elm$string('# Hello'),
				jN: $author$project$Interactive$string
			})
		]),
	bg: F2(
		function (_v1, foundParser) {
			var model = _v1._;
			var onChange = _v1.kb;
			return A2(
				$mdgriffith$elm_codegen$Elm$Case$result,
				A2(
					$author$project$Gen$Parser$call_.ku,
					foundParser,
					A2($mdgriffith$elm_codegen$Elm$get, 'source', model)),
				{
					jn: A2(
						$elm$core$Tuple$pair,
						'err',
						function (err) {
							return A2(
								$author$project$Gen$Element$el,
								_List_fromArray(
									[
										A2($author$project$Gen$Element$paddingXY, 32, 0),
										$author$project$Gen$Element$width($author$project$Gen$Element$fill),
										$author$project$Gen$Element$htmlAttribute(
										A2($author$project$Gen$Html$Attributes$style, 'background', 'rgb(36,36,36)'))
									]),
								$author$project$Gen$Ui$call_.eO(
									A2(
										$mdgriffith$elm_codegen$Elm$apply,
										$mdgriffith$elm_codegen$Elm$value(
											{
												bx: $elm$core$Maybe$Nothing,
												ei: _List_fromArray(
													['Debug']),
												ao: 'toString'
											}),
										_List_fromArray(
											[err]))));
						}),
					ka: A2(
						$elm$core$Tuple$pair,
						'ok',
						function (ok) {
							return A2(
								$author$project$Gen$Element$el,
								_List_fromArray(
									[
										A2($author$project$Gen$Element$paddingXY, 32, 0),
										$author$project$Gen$Element$width($author$project$Gen$Element$fill),
										$author$project$Gen$Element$htmlAttribute(
										A2($author$project$Gen$Html$Attributes$style, 'background', 'rgb(36,36,36)'))
									]),
								$author$project$Gen$Ui$call_.eO(
									A2(
										$mdgriffith$elm_codegen$Elm$apply,
										$mdgriffith$elm_codegen$Elm$value(
											{
												bx: $elm$core$Maybe$Nothing,
												ei: _List_fromArray(
													['Debug']),
												ao: 'toString'
											}),
										_List_fromArray(
											[ok]))));
						})
				});
		})
};
var $author$project$Exemplar$interactiveAll = function (mod) {
	var examples = A3(
		$elm$core$List$foldl,
		F2(
			function (val, exes) {
				if ($author$project$Example$Type$isStartingPoint(val.ay)) {
					var _v0 = A2($author$project$Example$Build$getValueNamed, val.ao, mod.k$);
					if (_v0.$ === 1) {
						return exes;
					} else {
						var value = _v0.a;
						var builtResult = A2(
							$author$project$Example$Interactive$build,
							mod,
							{
								gR: _List_fromArray(
									[$author$project$Exemplar$element, $author$project$Exemplar$parser]),
								t: value
							});
						if (builtResult.$ === 1) {
							var err = builtResult.a;
							return exes;
						} else {
							var examp = builtResult.a;
							return A2($elm$core$List$cons, examp, exes);
						}
					}
				} else {
					return exes;
				}
			}),
		_List_Nil,
		mod.k$);
	return $elm$core$Result$Ok(
		{c2: examples, ao: mod.ao});
};
var $author$project$Generate$renderExampleModule = function (mod) {
	var _v0 = $author$project$Exemplar$interactiveAll(mod);
	if (_v0.$ === 1) {
		var err = _v0.a;
		return $elm$core$Maybe$Just(
			A2(
				$mdgriffith$elm_codegen$Elm$file,
				_List_fromArray(
					['Live']),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_codegen$Elm$declaration,
						'error',
						$mdgriffith$elm_codegen$Elm$string(err))
					])));
	} else {
		var interactives = _v0.a;
		return A2(
			$author$project$Interactive$generate,
			_List_fromArray(
				['Live']),
			_List_fromArray(
				[interactives]));
	}
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Generate$main = A2(
	$author$project$Gen$CodeGen$Generate$fromJson,
	$elm$json$Json$Decode$list($elm$project_metadata_utils$Elm$Docs$decoder),
	$elm$core$List$filterMap($author$project$Generate$renderExampleModule));
_Platform_export({'Generate':{'init':$author$project$Generate$main($elm$json$Json$Decode$value)(0)}});}(this));//
const flags = JSON.parse(process.argv[2]);

async function generate(elmModule) {
  const promise = new Promise((resolve, reject) => {
    // @ts-ignore
    const app = elmModule.init({ flags: flags });
    if (app.ports.onSuccessSend) {
      app.ports.onSuccessSend.subscribe((val) => {
        console.log(JSON.stringify(val));
        resolve(val);
      });
    }
    if (app.ports.onInfoSend) {
      app.ports.onInfoSend.subscribe((info) => console.log(info));
    }
    if (app.ports.onFailureSend) {
      app.ports.onFailureSend.subscribe(reject);
    }
  });

  return await promise;
}

return generate(this.Elm.Generate);
