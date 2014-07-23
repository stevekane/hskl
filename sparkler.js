var cu        = require("lodash.curry")
var co        = require("lodash.compose")
var clone     = require("lodash.clone")
var flip      = function (fn) { return function (a, b) { return fn(b, a) } }
var cflip     = co(cu, flip)
var head      = function (li) { return li[0] }
var add       = cu(function (a,b) { return a + b })
var mult      = cu(function (a,b) { return a * b })
var gte       = cu(function (x, y) { return y >= x })
var isString  = function (str) { return typeof str === "string" }
var empty     = function (li) { return li.length === 0 }

var max = function {
  [x, ...xs] if empty(xs)   => x,
  [x, ...xs] if x > max(xs) => x,
  [x, ...xs]                => max(xs)
}

var cons = cu(function {
  (li, el) if isString(li) => add(el, li),
  (li, el)                 => {
    var liCpy = clone(li)
      , elCpy = clone(el)   

    liCpy.unshift(elCpy)
    return liCpy
  }
})

var foldl = cu(function {
  (f, s, xs) if empty(xs) => s,
  (f, s, [x, ...xs])      => foldl(f, f(s, x), xs)
})

var foldr = cu(function {
  (f, s, xs) if empty(xs) => s,
  (f, s, [...xs, x])      => foldr(f, f(x, s), xs)
})

var map = cu(function (f, li) {
  var accumFn = function (x, acc) { return cons(acc, f(x)) }

  return foldr(accumFn, [], li)
})

var filter = cu(function (f, li) {
  var boolFn = function (x, acc) { return f(x) ? cons(acc, x) : acc }

  return foldr(boolFn, [], li)
});

var elem = cu(function (el, li) {
  var boolFn = function (acc, x) { return acc || el === x }

  return foldl(boolFn, false, li)
})

//RPN THINGY
var removeSpaces = function (str) { return str.split(" ") }
var getResult    = head
var sum          = foldl(add, 0)
var prod         = foldl(mult, 1)

var updateStack = function {
  ([fst, sec, ...r], "*") => cons(r, (fst * sec)),
  ([fst, sec, ...r], "/") => cons(r, (sec / fst)),
  ([fst, sec, ...r], "+") => cons(r, (fst + sec)),
  ([fst, sec, ...r], "-") => cons(r, (sec - fst)),
  ([fst, sec, ...r], "^") => cons(Math.pow(sec, fst), r),
  ([fst, _, ...r], "ln")  => cons(Math.log(fst), r),
  (stack, "sum")          => [sum(stack)],
  (stack, "prod")         => [prod(stack)],
  (stack, num)            => cons(stack, Number(num))
}

var processStack = foldl(updateStack, [])

var solveRpn = co(getResult, processStack, removeSpaces)

console.log(max([1,2,3]))
console.log(cons([2,3,4], 1));
console.log(cons("teven", "s"));
console.log(foldl(add, 0, [1,2,3,4]))
console.log(foldr(add, 0, [1,2,3,4]))
console.log(map(add(1), [1,2,3,4]))
console.log(filter(gte(3), [1,2,3,4,5,6]))
console.log(elem(3, [1,2,3,4]))
console.log(solveRpn("1 2 3 sum 5 +"))
