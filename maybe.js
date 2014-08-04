function log (thing) {
  console.log(thing.toString());
};

//Constructors for Maybe
function Just (val) { 
  this.val = val
}
Just.prototype.toString = function () {
  return "Just " + this.val
};
Just.prototype.bindM    = function (fnAToMb) { 
  return fnAToMb(this.val)
}
Just.prototype.returnM  = returnMaybe

function Nothing () {}
Nothing.prototype.toString = function () {
  return "Nothing"
}
Nothing.prototype.bindM    = function (fnAToMb) { 
  return new Nothing
}
Nothing.prototype.returnM  = returnMaybe

//free standing definitions of these functions
function returnMaybe (x) { return new Just(x) }
function bindMaybe (mA, fnAToMb) {
  if (mA instanceof Nothing)   return new Nothing
  else if (mA instanceof Just) return fnAToMb(mA.val)
  else                         throw new Error("not Maybe constructor")
}

function returnNothing () { return new Nothing }

function gt4 (x) { return x > 4 ? new Just(x) : new Nothing }

var nothing  = new Nothing
var just5    = new Just(5)
var bigChain = just5.bindM(returnMaybe).bindM(gt4).bindM(returnMaybe)

log(returnMaybe(3))
log(bindMaybe(nothing, returnMaybe))
log(bindMaybe(just5, returnMaybe))

log(nothing.bindM(returnMaybe))
log(just5.bindM(returnMaybe))
log(bigChain)
