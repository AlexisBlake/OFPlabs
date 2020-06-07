package Lab1

object Lab1 {

  val arith: (Int => Boolean) => (Int, Int, Int) => List[Int] =
    f => {
      case (0, _, _) => List()
      case (n, a0, d) if f(a0) => a0 :: arith(f)(n - 1, a0 + d, d)
      case (n, a0, d) => arith(f)(n, a0 + d, d)
    }

  val check: Int => Boolean =
    (a) => if (a*2 > 15) true else false

  def main(args: Array[String]): Unit = {
    println(arith(check)(4, 2, 5))
  }
}
