package Lab3

abstract class BoolCalc[T] {
  def neg(a: T): T
  def and(a: T, b: T): T
  def or(a: T, b: T): T
  def f(): T
}

object BoolCalc {
  implicit object boolean extends BoolCalc[Boolean] {
    override def neg(a: Boolean): Boolean = !a
    override def and(a: Boolean, b: Boolean): Boolean = a && b
    override def or(a: Boolean, b: Boolean): Boolean = a || b
    override def f(): Boolean = false
  }
}
