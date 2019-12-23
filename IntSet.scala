
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def filterAcc(p: Int => Boolean, acc: IntSet): IntSet
  def union(that: IntSet): IntSet
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  def filterAcc(p: Int => Boolean, acc: IntSet): IntSet = acc
  override def toString = "."
  def union(that: IntSet): IntSet = that
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  override def toString = "{" + left + elem + right + "}"
  def filterAcc(p: Int => Boolean, acc: IntSet): IntSet = {
    //println("elem:" + elem + " cur acc:" + acc)
    if (p(elem)) right.filterAcc(p, left.filterAcc(p, acc).incl(elem))
    else right.filterAcc(p, left.filterAcc(p, acc))
  }
  def always_true(x: Int): Boolean = true
  def union(that: IntSet): IntSet = filterAcc((x: Int) => true, that)

}

def pred_odd(input: Int): Boolean = {
  if (input % 2 == 1) true
  else false
}

val v0 = new Empty
val v1 = v0 incl 3
val v2 = v1 incl 2
val v3 = v2 incl 1
val v4 = v3 incl 4

val vv0 = new Empty
val vv1 = vv0 incl 33
val vv2 = vv1 incl 22

//println(v4)
//println(v4.filterAcc(pred_odd, new Empty))
println(v2)
println(vv2)
println(v2.union(vv2))

println(v2)
println(vv2)
println(v0.union(v2))
