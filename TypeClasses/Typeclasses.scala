package LAB4

import LAB4.Typeclasses.Reversable.ReversableLikeString
import LAB4.Typeclasses.Smash.{SmashDouble, SmashInt, SmashString}


object Typeclasses {

  object Reversable {
    trait Reversable[T] {
      def reverse(a: T): T
    }

    def reverse[T: Reversable](a: T): T = {
      implicitly[Reversable[T]].reverse(a)
    }

    implicit object ReversableLikeString extends Reversable[String] {
      override def reverse(str: String): String = str.reverse
    }
  }
  def testReversableString(str: String): String = Reversable.reverse(str)

  object Smash {
    trait Smash[T] {
      def smash(a: T, b: T): T
    }

    def smash[T: Smash](a: T, b: T): T = {
      implicitly[Smash[T]].smash(a, b)
    }

    implicit object SmashInt extends Smash[Int] {
      override def smash(a: Int, b: Int): Int = a + b
    }
    def testSmashInt(a: Int, b: Int): Int = Smash.smash(a, b)

    implicit object SmashDouble extends Smash[Double] {
      override def smash(a: Double, b: Double): Double = a * b
    }
    def testSmashDouble(a: Double, b: Double): Double = Smash.smash(a, b)

    implicit object SmashString extends Smash[String] {
      override def smash(a: String, b: String): String = a.concat(b)
    }
    def testSmashString(a: String, b: String): String = Smash.smash(a, b)
  }

  def main(args: Array[String]): Unit = {
    var test1 = ReversableLikeString
    var test2 = SmashInt
    var test3 = SmashDouble
    var test4 = SmashString
    println(test1.reverse("Scala is hard"))
    println(test2.smash(123, 321))
    println(test3.smash(12.3, 32.1))
    println(test4.smash("Web", "Code"))
  }
}
