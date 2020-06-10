

import scala.annotation.tailrec
import scala.collection.immutable.List

/** Реализуйте функции для решения следующих задач.
 * Примечание: Попытайтесь сделать все функции с хвостовой рекурсией, используйте аннотацию для подстверждения.
 * рекурсия будет хвостовой если:
 *   1. рекурсия реализуется в одном направлении
 *   2. вызов рекурсивной функции будет последней операцией перед возвратом
 */
object RecursiveFunctions  extends App {

  def length[A](as: List[A]): Int = {
    @tailrec
    def loop(rem: List[A], agg: Int): Int = rem match {
      case x :: tail  => loop(tail, agg + 1)
      case Nil         => agg
    }

    loop(as, 0)
  }

  /* a) Напишите функцию которая записывает в обратном порядке список:
   *        def reverse[A](list: List[A]): List[A]
   */
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def loop(rem: List[A], result: List[A]): List[A] = rem match {
      case x :: tail  =>  loop(tail, x :: result)
      case Nil         => result
    }
    loop(list, Nil)
  }

  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testReverse[A](list: List[A]): List[A] = reverse(list)

  /* b) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def map[A, B](list: List[A])(f: A => B): List[B]
   */

  def map[A, B](list: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(rem: List[A], result: List[B])(f: A => B): List[B] = rem match {
      case x :: tail => loop(tail, result :+ f(x))(f)
      case Nil       => result
    }
    loop(list, Nil)(f)
  }

  // используйте функцию из пункта  (b) здесь, не изменяйте сигнатуру
  def testMap[A, B](list: List[A], f: A => B): List[B] = map(list)(f)

  /* c) Напишите функцию, которая присоединяет один список к другому:
   *        def append[A](l: List[A], r: List[A]): List[A]
   */

  def Append[A](l: List[A], r: List[A]) : List[A] = {
    @tailrec
    def loop(rem: List[A], result: List[A]) : List[A] = rem match {
      case x :: tail => loop(tail, result :+ x)
      case Nil       => result
    }
    loop(r, l)
  }

  // используйте функцию из пункта  (c) здесь, не изменяйте сигнатуру
  def testAppend[A](l: List[A], r: List[A]): List[A] = Append(l, r)

  /* d) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def flatMap[A, B](list: List[A])(f: A => List[B]): List[B]
   *
   *    она получает функцию, которая создает новый List[B] для каждого элемента типа A в
   *    списке. Поэтому вы создаете List[List[B]].
   */

  def FlatMap[A, B](list: List[A])(f: A => List[B]): List[List[B]] = {
    @tailrec
    def loop(rem: List[A], result: List[List[B]])(f: A => List[B]): List[List[B]] = rem match {
      case x :: tail => loop(tail, result :+ f(x))(f)
      case Nil       => result
    }
    loop(list, Nil)(f)
  }

  // используйте функцию из пункта  (d) здесь, не изменяйте сигнатуру
  def testFlatMap[A, B](list: List[A], f: A => List[B]): List[List[B]] = FlatMap(list)(f)

/* e) Вопрос: Возможно ли написать функцию с хвостовой рекурсией для `Tree`s? Если нет, почему? */
//Нет т.к. хвостовая рекурсия работает только в одну сторону


  println(testReverse(List(1, 2, 3,1,2,4)))
}
