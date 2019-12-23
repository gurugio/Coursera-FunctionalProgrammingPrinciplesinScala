// Start writing your ScalaFiddle code here
type FunSet = Int => Boolean
def contains(s: FunSet, elem: Int): Boolean = s(elem)
def singletonSet(elem: Int): FunSet = (x: Int) => x == elem


var s1 = singletonSet(1)
var s2 = singletonSet(2)

def union(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) || t(x)
def intersect(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) && t(x)
def diff(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) && !t(x)

val s = union(s1, s2)
assert(contains(s, 1), "Union 1")
assert(contains(s, 2), "Union 2")
assert(!contains(s, 3), "Union 3")
val ds = diff(s1, s2)
assert(contains(ds, 1), "diff 1")
assert(!contains(ds, 2), "diff 2")
var is1 = singletonSet(1)
var is2 = singletonSet(1)
val is = intersect(is1, is2)
assert(contains(is, 1), "intersect 1")
assert(!contains(is, 2), "intersect 2")
var s3 = singletonSet(3)
val ss = union(s, s3)
assert(contains(ss, 3), "union ss")
assert(contains(ss, 2), "union ss")
assert(contains(ss, 1), "union ss")
assert(!contains(ss, 4), "union ss")

def filter(s: FunSet, p: Int => Boolean): FunSet = (x: Int) => s(x) && p(x)
var ff = filter(ss, (n: Int) => n == 2)
assert(!ff(1), "filter 1")
assert(ff(2), "filter 2")

val bound = 1000

/**
 * Returns whether all bounded integers within `s` satisfy `p`.
 */
def forall(s: FunSet, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > bound) true
    else if (s(a) && !p(a)) false
    else iter(a + 1)
  }
  iter(-bound)
}

var num2 = singletonSet(2)
var num4 = singletonSet(4)
var num6 = singletonSet(6)
var setEven = union(union(num2, num4), num6)

assert(forall(setEven, (x: Int) => x % 2 == 0), "forall 1")
assert(!forall(setEven, (x: Int) => x % 2 == 1), "forall 2")

def exists(s: FunSet, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > bound) false
    else if (s(a) && p(a)) true
    else iter(a + 1)
  }
  iter(-bound)
}
assert(exists(setEven, (x: Int) => x == 4), "exists 1")
assert(!exists(setEven, (x: Int) => x == 5), "exists 2")
/**
 * Displays the contents of a set
 */
def toString(s: FunSet): String = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.mkString("{", ",", "}")
}

/**
 * Prints the contents of a set on the console.
 */
def printSet(s: FunSet): Unit = {
  println(toString(s))
}

printSet(setEven)

def map(s: FunSet, f: Int => Int): FunSet = {
  var ret = singletonSet(-bound - 1)
  for (i <- -bound to bound if contains(s, i)) yield ret = union(ret,
singletonSet(f(i)))
  ret
}

printSet(map(setEven, (x: Int) => x + 1))

println("END")

