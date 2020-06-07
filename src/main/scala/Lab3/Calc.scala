package Lab3

abstract class Calc[T] {
  def plus(a: T, b: T): T
  def minus(a: T, b: T): T
  def div(a: T, b: T): T
  def mul(a: T, b: T): T
}

object Calc {

  implicit object int extends Calc[Int] {
    override def plus(a: Int, b: Int): Int = a + b
    override def minus(a: Int, b: Int): Int = a - b
    override def div(a: Int, b: Int): Int = a / b
    override def mul(a: Int, b: Int): Int = a * b
  }

  implicit object double extends Calc[Double] {
    override def plus(a: Double, b: Double): Double = a + b
    override def minus(a: Double, b: Double): Double = a - b
    override def div(a: Double, b: Double): Double = a / b
    override def mul(a: Double, b: Double): Double = a * b
  }

  implicit object float extends Calc[Float] {
    override def plus(a: Float, b: Float): Float = a + b
    override def minus(a: Float, b: Float): Float = a - b
    override def div(a: Float, b: Float): Float = a / b
    override def mul(a: Float, b: Float): Float = a * b
  }

  implicit object long extends Calc[Long] {
    override def plus(a: Long, b: Long): Long = a + b
    override def minus(a: Long, b: Long): Long = a - b
    override def div(a: Long, b: Long): Long = a / b
    override def mul(a: Long, b: Long): Long = a * b
  }

  implicit object short extends Calc[Short] {
    override def plus(a: Short, b: Short): Short = (a + b).toShort
    override def minus(a: Short, b: Short): Short = (a - b).toShort
    override def div(a: Short, b: Short): Short = (a / b).toShort
    override def mul(a: Short, b: Short): Short = (a * b).toShort
  }

  implicit object byte extends Calc[Byte] {
    override def plus(a: Byte, b: Byte): Byte = (a + b).toByte
    override def minus(a: Byte, b: Byte): Byte = (a - b).toByte
    override def div(a: Byte, b: Byte): Byte = (a / b).toByte
    override def mul(a: Byte, b: Byte): Byte = (a * b).toByte
  }

}
