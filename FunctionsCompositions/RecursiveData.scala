import scala.collection.immutable.List

object RecursiveData extends App {

  // a) Реализуйте функцию, определяющую является ли пустым `List[Int]`.
  def ListIntEmpty(list: List[Int]) : Boolean = list match {
    case x :: tail => true
    case Nil         => false
  }

  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntEmpty(list: List[Int]): Boolean = ListIntEmpty(list)

  // b) Реализуйте функцию, которая получает head `List[Int]`или возвращает -1 в случае если он пустой.
  def ListIntHead(list: List[Int]) : Int = list match {
    case x :: tail => x
    case Nil         => -1
  }

  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntHead(list: List[Int]): Int = ListIntHead(list)

  // c) Можно ли изменить `List[A]` так чтобы гарантировать что он не является пустым?
  def ListNotEmpty[A](head: A, list: List[A]) : List[A] = list match {
    case Nil => head :: list
    case x :: tail => list
  }

  /* d) Реализуйте универсальное дерево (Tree) которое хранит значения в виде листьев и состоит из:
   *      node - левое и правое дерево (Tree)
   *      leaf - переменная типа A
   */
  class Tree[A](LeftNode: Tree[A], RightNode: Tree[A], leaf: A)
  {
  }

  println(ListNotEmpty(1, List(2)))
}
