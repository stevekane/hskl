let {curry, partial, clone, compose} = require("lodash")
let cu       = curry
let pa       = partial
let co       = compose
let flip     = (fn) => { return (a, b) => { return fn(b,a) } }
let cflip    = co(curry, flip)
let tail     = li => li.slice(1)
let head     = li => li[0]
let last     = li => li[li.length -1]
let butlast  = li => li.slice(0, li.length -1)
let empty    = li => li.length === 0
let isString = li => typeof li === "string"
let slice    = cu((start, end, li) => li.slice(start, end))
let rest     = cu((start, li) => li.slice(start))
let words    = li => li.split(" ")
let concat   = cu((li, li2) => li.concat(li2), 2)
let dot      = cu((key, obj) => obj[key])
let add      = cu((x, y) => x +  y)
let sub      = cu((x, y) => x -  y)
let mult     = cu((x, y) => x *  y)
let div      = cu((x, y) => x /  y)
let or       = cu((x, y) => x || y)
let and      = cu((x, y) => x && y)
let gt       = cu((x, y) => y >  x)
let lt       = cu((x, y) => y <  x)
let gte      = cu((x, y) => y >= x)
let lte      = cu((x, y) => y <= x)

const LTR_LOWER = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
                   'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
const LTR_UPPER = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                   'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']
const NUMBERS   = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
const SPACE     = ' '
const BREAK     = '\n'
const LETTERS   = concat(LTR_LOWER, LTR_UPPER)
const ALPHANUM  = concat(LETTERS, NUMBERS)

let consStr = cflip(add)
let consArr = cu((li, el) => {
  let liCpy = clone(li)
    , elCpy = clone(el)

  liCpy.unshift(elCpy)
  return liCpy
});
let cons   = cu((li, el) => isString(li) ? consStr(li, el) : consArr(li, el))

let foldl  = cu((f, s, li) => empty(li) ? s : foldl(f, f(s, head(li)), tail(li)))

let foldr  = cu((f, s, li) => empty(li) ? s : foldr(f, f(last(li), s), butlast(li)))

let map    = cu((f, li) => {
  let accumFn = (x, acc) => cons(acc, f(x))

  return foldr(accumFn, [], li)
})

let filter = cu((f, li) => {
  let boolFn = (x, acc) => f(x) ? cons(acc, x) : acc

  return foldr(boolFn, [], li)
})

let elem   = cu((el, li) => {
  let boolFn = (acc, x) => acc || el === x

  return foldl(boolFn, false, li)
})

let max  = (li) => {
  if      (empty(li))                return new Error("max of empty list")
  else if (empty(tail(li)))          return head(li)
  else if (head(li) > max(tail(li))) return head(li)
  else                               return max(tail(li))
};

let replicate = cu((c, el) => c <= 0 ? [] : cons(replicate(c-1, el), el))

let take = cu((c, li) => {
  if      (c<=0)      return isString(li) ? "" : []
  else if (empty(li)) return isString(li) ? "" : []
  else                return cons(take(c-1, tail(li)), head(li))
})

let tails = (li) => empty(li) ? [] : cons(tails(tail(li), li))

let search = cu((sub, li) => {
  let subLen  = len(sub)
    , allTs   = tails(li)
    , checkFn = (acc, x) => take(subLen, x) === sub ? true : acc

  return foldl(checkFn, false, allTs)
})

let all = cu((predFn, li) => { 
  let boolFn = (x, acc) => predFn(x)

  return foldr(boolFn, false, li)
})

let any = cu((predFn, li) => {
  let boolFn = (x, acc) => acc || predFn(x)

  return foldr(boolFn, false, li)
})

let fmap    = map
let len     = foldl(add(1), 0)
let sum     = foldl(add, 0)
let prod    = foldl(mult, 1)
let reverse = foldl(cons, [])
let ofList  = cflip(elem)
let isLower = ofList(LTR_LOWER)
let isUpper = ofList(LTR_UPPER)
let isAlpha = ofList(LETTERS)
let isNum   = ofList(NUMBERS)
let isAlNum = ofList(ALPHANUM)
let isBreak = ofList(BREAK)

//examples and stuff
//RPN CALCULATOR
let removeSpaces = words

let getResult    = head

let updateStack = (stack, str) => {
  let fst  = stack[0]
    , sec  = stack[1]
    , r    = rest(2, stack)

  switch (str) {
    case "*"    : return cons(r, (fst * sec)); 
    case "/"    : return cons(r, (sec / fst)); 
    case "+"    : return cons(r, (fst + sec)); 
    case "-"    : return cons(r, (sec - fst)); 
    case "^"    : return cons(Math.pow(sec, fst), r); 
    case "ln"   : return cons(Math.log(fst), r); 
    case "sum"  : return [sum(stack)];
    case "prod" : return [prod(stack)];
    default     : return cons(stack, Number(str));
  }
}

let processStack = foldl(updateStack, [])

let solveRpn = co(getResult, processStack, removeSpaces)

//SHORTEST-PATH


console.log(solveRpn("1 2 +"));
console.log(solveRpn("1 2 3 4 + * -"));
console.log(solveRpn("1 2 3 4 sum"));
console.log(solveRpn("1 2 3 4 prod"));
