package Lab2

class Polynomial (inputCoefs: Array[Int]) {
  val coefs: Array[Int] = inputCoefs

  def * (d: Int): Polynomial = {
    val coefsNew: Array[Int] = new Array[Int](coefs.length)
    for (i <- coefs.indices) {
      coefsNew(i) = coefs(i) * d
    }
    new Polynomial(coefsNew)
  }

  def * (p: Polynomial): Polynomial = {
    val size = coefs.length + p.coefs.length - 1
    val coefsNew: Array[Int] = new Array[Int](size)
    coefsNew.init(0)

    for (i <- coefs.indices) {
      for (j <- p.coefs.indices) {
        coefsNew(i + j) += coefs(i) * p.coefs(j)
      }
    }

    new Polynomial(coefsNew)
  }

  def + (d: Int): Polynomial = {
    val coefsNew: Array[Int] = coefs
    coefsNew(0) += d
    new Polynomial(coefsNew)
  }

  def + (p: Polynomial): Polynomial = {
    var size = 0
    if (coefs.length >= p.coefs.length)
      size = coefs.length
    else
      size = p.coefs.length
    val coefsNew: Array[Int] = new Array[Int](size)
    coefsNew.init(0)

    if (size == coefs.length) {
      for (i <- p.coefs.indices) {
        coefsNew(i) = coefs(i) + p.coefs(i)
      }
      for (i <- p.coefs.length until coefs.length) {
        coefsNew(i) = coefs(i)
      }
    } else {
      for (i <- coefs.indices) {
        coefsNew(i) = coefs(i) + p.coefs(i)
      }
      for (i <- coefs.length until p.coefs.length) {
        coefsNew(i) = p.coefs(i)
      }
    }

    new Polynomial(coefsNew)
  }

  def unary_! (): Polynomial = {
    val coefsNew: Array[Int] = new Array[Int](coefs.length - 1)
    coefsNew.init(0)

    for (i <- coefsNew.indices) {
      coefsNew(i) = coefs(i + 1) * (i + 1)
    }

    new Polynomial(coefsNew)
  }

  private def aboveZero(d: Int): Boolean = {
    if (d < 0) false else true
  }

  override def toString: String = {
    val s:StringBuilder = new StringBuilder()
    var count = 0
    for (elem <- coefs) {
      if (count > 0) {
        if (aboveZero(elem)) {
          if (elem != 0) {
            s.insert(0, " + " + elem + "x^" + count)
          }
        } else {
          s.insert(0, " - " + elem.abs + "x^" + count)
        }
      } else {
        if (aboveZero(elem)) {
          if (elem != 0) {
            s.insert(0, " + " + elem)
          }
        } else {
          s.insert(0, " - " + elem.abs)
        }
      }
      count += 1
    }
    if (s.charAt(1) == '+') {
      s.delete(0, 3)
    } else {
      s.delete(0, 3)
      s.insert(0, "-")
    }
    s.toString()
  }
}

object Lab2 {

  def main(args: Array[String]): Unit = {
    val m1:Array[Int] = Array(1, 2, -3, 4)
    val p = new Polynomial(m1)
    println("p = \t\t\t\t\t\t" + p)
    println("p * -2 = \t\t\t\t\t" + p * -2)
    println("p * (p * -2) = \t\t\t\t" + p * (p * -2))
    println("p + 5 = \t\t\t\t\t" + (p + 5))
    println("(p + 5) * 3 = \t\t\t\t" + p * 3)
    println("(p + 5) + (p + 5) * 3 = \t" + (p + p * 3))
    println("!(p + 5) = \t\t\t\t\t" + !p)
  }
}