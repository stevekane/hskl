var curry    = require("lodash.curry");
var partial  = require("lodash.partial");
var clone    = require("lodash.cloneDeep");
var compose  = require("lodash.compose");
var flip     = function (fn) { return function (a, b) { return fn(b,a) } };
var cflip    = compose(curry, flip);
var tail     = function (li) { return li.slice(1) };
var head     = function (li) { return li[0] };
var last     = function (li) { return li[li.length -1] };
var butlast  = function (li) { return li.slice(0, li.length -1) };
var empty    = function (li) { return li.length === 0 };
var isString = function (li) { return typeof li === "string" };
var slice    = curry(function (start, end, li) { return li.slice(start, end) });
var concat   = curry(function (li, li2) { return li.concat(li2); }, 2);
var dot      = curry(function (key, obj) { return obj[key] });
var add      = curry(function (x, y) { return x +  y });
var sub      = curry(function (x, y) { return x -  y });
var mult     = curry(function (x, y) { return x *  y });
var div      = curry(function (x, y) { return x /  y });
var or       = curry(function (x, y) { return x || y });
var and      = curry(function (x, y) { return x && y });
var gt       = curry(function (x, y) { return y >  x });
var lt       = curry(function (x, y) { return y <  x });
var gte      = curry(function (x, y) { return y >= x });
var lte      = curry(function (x, y) { return y <= x });

//some useful constants
var LTR_LOWER = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
                 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];
var LTR_UPPER = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'];
var NUMBERS   = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
var SPACE     = ' ';
var BREAK     = '\n';
var LETTERS   = concat(LTR_LOWER, LTR_UPPER);
var ALPHANUM  = concat(LETTERS, NUMBERS);

var consStr = cflip(add);

//force cloning when adding element to an array
var consArr = curry(function consArr (li, el) {
  var liCpy = clone(li);
  var elCpy = clone(el);

  liCpy.unshift(elCpy);
  return liCpy;
});

var cons = curry(function cons (li, el) {
  if      (isString(li)) return consStr(li, el);
  else                   return consArr(li, el);
});

var foldl = curry(function foldl (f, start, li) {
  if      (empty(li)) return start;
  else                return foldl(f, f(start, head(li)), tail(li));
});

var foldr = curry(function foldr (f, start, li) {
  if      (empty(li)) return start;
  else                return foldr(f, f(last(li), start), butlast(li));
});

//f :: [b] -> [a]
var map       = curry(function map (f, li) {
  var accumFn = function (x, acc) { return cons(acc, f(x)) };
  
  return foldr(accumFn, [], li);
});

//f :: a -> Boolean
var filter   = curry(function filter (f, li) {
  var boolFn = function (x, acc) { return f(x) ? cons(acc, x) : acc };

  return foldr(boolFn, [], li);
});

//f :: a -> Boolean
var elem     = curry(function elem (el, li) { 
  var boolFn = function (acc, x) { return acc || el === x };

  return foldl(boolFn, false, li);
});

var fmap    = map;
var len     = foldl(add(1), 0);
var sum     = foldl(add, 0);
var reverse = foldl(cons, []);
var ofList  = cflip(elem);
var isLower = ofList(LTR_LOWER);
var isUpper = ofList(LTR_UPPER);
var isAlpha = ofList(LETTERS);
var isNum   = ofList(NUMBERS);
var isAlNum = ofList(ALPHANUM);
var isBreak = ofList(BREAK);

var max = function max (list) {
  if      (empty(list))                  throw new Error("max of empty list");
  else if (empty(tail(list)))            return head(list);
  else if (head(list) > max(tail(list))) return head(list);
  else                                   return max(tail(list));
};

var replicate = curry(function replicate (count, el) {
  if      (count <= 0) return [];
  else                 return cons(replicate(count-1, el), el);
});

var take = curry(function take (count, list) {
  if      (count <=0)   return isString(list) ? "" : [];
  else if (empty(list)) return isString(list) ? "" : [];
  else                  return cons(take(count-1, tail(list)), head(list)); 
});

var applyTimes = curry(function applyTimes (count, fn, arg) {
  if      (count === 0) return new Error("cannot apply fn zero times");
  else if (count === 1) return fn(arg);
  else                  return fn(applyTimes(count-1, fn, arg));
});

var tails = function tails (li) {
  if      (empty(li)) return [];
  else                return cons(tails(tail(li)), li);
};

var search   = curry(function search (sub, li) {
  var subLen = len(sub);
  var allTs  = tails(li);
  var check  = function (acc, x) { 
    return take(subLen, x) === sub ? true : acc;
  };

  return foldl(check, false, allTs);
});

var all = curry(function all (predFn, li) {
  var boolFn = function (x, acc) { return predFn(x) };
  
  return foldr(boolFn, false, li);
});

var any = curry(function any (predFn, li) {
  var boolFn = function (x, acc) { return acc || predFn(x) };

  return foldr(boolFn, false, li);
});


console.log("map add(1) over [1,2,3,4]");
console.log(map(add(1), [1,2,3,4]));

console.log("filter gt(2) over [1,2,3,4]");
console.log(filter(gt(2), [1,2,3,4]));

console.log("len of [1,2,3,4]");
console.log(len([1,2,3,4]));

console.log("sum of [1,2,3,4]");
console.log(sum([1,2,3,4]));

console.log("max of [1,2,3,4]");
console.log(max([1,2,3,4]));

console.log("replicate 3 of 5");
console.log(replicate(3, 5));

console.log("take 3 of [1,2,3,4]");
console.log(take(3, [1,2,3,4]));

console.log("reverse [1,2,3,4]");
console.log(reverse([1,2,3,4]));

console.log("elem 1 [1,2,3,4]");
console.log(elem(1, [1,2,3,4]));

console.log("isLower of 'a'");
console.log(isLower('a'));

console.log("isUpper of 'a'");
console.log(isUpper('a'));

console.log("isAlpha of 'a'");
console.log(isAlpha('a'));

console.log("isNum of 'a'");
console.log(isNum('a'));

console.log("isAlNum of 'a'");
console.log(isAlNum('a'));

console.log("isBreak of '<NEWLINE CHAR>'");
console.log(isBreak('\n'));

console.log("applyTimes 3 reverse [1,2,3,4]");
console.log(applyTimes(3, reverse, [1,2,3,4]));

console.log("foldl(add, 0, [1,2,3,4])");
console.log(foldl(add, 0, [1,2,3,4]));

console.log("tails of 'steve kane'");
console.log(tails("steve kane"));

console.log("search of 'steve' in 'steve kane'");
console.log(search('steve', 'steve kane'));

console.log("all lt3 of [1,2,3,4]");
console.log(all(lt(5), [1,2,3,4]));

console.log("all gt2 of [1,2,3,4]");
console.log(all(gt(2), [1,2,3,4]));

console.log("any gt3 of [1,2,3,4]");
console.log(any(gt(3), [1,2,3,4]));

console.log("any gt10 of [1,2,3,4]");
console.log(any(gt(10), [1,2,3,4]));
