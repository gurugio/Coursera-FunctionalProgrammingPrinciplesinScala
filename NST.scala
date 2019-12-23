// Start writing your ScalaFiddle code here
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new Error("zero.predecessor")
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = {
    if (that.isZero) this
    else throw new Error("zero.negative")
  }
  override def toString = "Zero"
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def successor: Nat = this.successor
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat = n - that.predecessor
  override def toString = "{" + n + "+1}"
}

val One = new Succ(Zero)
println(Zero)
println(One)
val Two = One + One
println(Two)

println(One + Two)

