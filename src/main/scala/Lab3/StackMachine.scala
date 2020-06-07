package Lab3

import scala.collection.mutable
import scala.collection.mutable.Stack

class StackMachine[T] (val stack: Stack[T]) {

  def plus() (implicit calc: Calc[T]): T = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      val sum = calc.plus(a, b)
      stack.push(sum)
      sum
    } else {
      val a = stack.pop()
      stack.push(a)
      a
    }
  }

  def minus() (implicit calc: Calc[T]): T = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      val dif = calc.minus(a, b)
      stack.push(dif)
      dif
    } else {
      val a = stack.pop()
      stack.push(a)
      a
    }
  }

  def div() (implicit calc: Calc[T]): T = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      val priv = calc.div(a, b)
      stack.push(priv)
      priv
    } else {
      val a = stack.pop()
      stack.push(a)
      a
    }
  }

  def mul() (implicit calc: Calc[T]): T = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      val com = calc.mul(a, b)
      stack.push(com)
      com
    } else {
      val a = stack.pop()
      stack.push(a)
      a
    }
  }

  def and() (implicit boolCalc: BoolCalc[T]): T = {
    if (stack.nonEmpty) {
      val a = stack.pop()
      val b = stack.pop()
      val aAndB = boolCalc.and(a, b)
      stack.push(aAndB)
      aAndB
    } else {
      boolCalc.f()
    }
  }

  def or() (implicit boolCalc: BoolCalc[T]): T = {
    if (stack.nonEmpty) {
      val a = stack.pop()
      val b = stack.pop()
      val aAndB = boolCalc.or(a, b)
      stack.push(aAndB)
      aAndB
    } else {
      boolCalc.f()
    }
  }

  def neg() (implicit boolCalc: BoolCalc[T]): T = {
    if (stack.nonEmpty) {
      val a = stack.pop()
      val notA = boolCalc.neg(a)
      stack.push(notA)
      notA
    } else {
      boolCalc.f()
    }
  }
}

object Lab3 {

  def main(args: Array[String]): Unit = {
    val s1 = new mutable.Stack[Int]()
    s1.push(1, 2, 3, 4, 5)
    val sm1 = new StackMachine[Int](s1)
    println(sm1.plus())
    println(sm1.minus())
    println(sm1.div())
    println(sm1.mul())

    val s2 = new mutable.Stack[Double]()
    s2.push(1, 2, 3, 4, 5)
    val sm2 = new StackMachine[Double](s2)
    println(sm2.plus())
    println(sm2.minus())
    println(sm2.div())
    println(sm2.mul())

    val s3 = new mutable.Stack[Boolean]()
    s3.push(false, true, false, false)
    val sm3 = new StackMachine[Boolean](s3)
    println(sm3.neg())
    println(sm3.or())
    println(sm3.and())
    println(sm3.and())


  }
}