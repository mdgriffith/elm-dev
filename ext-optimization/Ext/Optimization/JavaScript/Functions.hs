{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Ext.Optimization.JavaScript.Functions
  ( functions
  )
  where


import qualified Data.ByteString.Builder as B
import Text.RawString.QQ (r)

import qualified Ext.Optimization.Level as Level


functions :: Level.Level -> B.Builder -> B.Builder
functions level defaultFunctions =
  case level of
    Level.O0 -> defaultFunctions
    Level.O2 -> optimizedFunctions
    Level.O3 -> optimizedFunctions


optimizedFunctions :: B.Builder
optimizedFunctions = [r|

function F2(fun) {
  var curried = function(a) { return function(b) { return fun(a,b); }; };
  curried.a2 = fun;
  return curried;
}
function F3(fun) {
  var curried = function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  };
  curried.a3 = fun;
  return curried;
}
function F4(fun) {
  var curried = function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  };
  curried.a4 = fun;
  return curried;
}
function F5(fun) {
  var curried = function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  };
  curried.a5 = fun;
  return curried;
}
function F6(fun) {
  var curried = function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  };
  curried.a6 = fun;
  return curried;
}
function F7(fun) {
  var curried = function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  };
  curried.a7 = fun;
  return curried;
}
function F8(fun) {
  var curried = function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  };
  curried.a8 = fun;
  return curried;
}
function F9(fun) {
  var curried = function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  };
  curried.a9 = fun;
  return curried;
}

function A2(fun, a, b) {
  return fun.a2 ? fun.a2(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a3 ? fun.a3(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a4 ? fun.a4(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a5 ? fun.a5(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a6 ? fun.a6(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a7 ? fun.a7(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a8 ? fun.a8(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a9 ? fun.a9(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

|]
