
// Примечание: напишите функции с хвостовой рекурсией

object Sequence extends App{

  /* a) Найдите последний элемент Seq.
   *
   */
  def LastElement[A](seq: Seq[A]): Option[A] =  Some(seq.last)

  def testLastElement[A](seq: Seq[A]): Option[A] = LastElement(seq)

  val list = List(1,2,3)
  val list2 = List(4,5,6)
  val list3 = List(4,5,4)
  println(testLastElement(list))

  /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.
   *
   */
  def Zip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = a.zip(b)

  def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = Zip(a, b)

  println(testZip(list,list2))

  /* c) Проверьте, выполняется ли условие для всех элементов в Seq.
   *
   */
  def ForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = seq.forall(cond)

  def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = ForAll(seq)(cond)

  /* d) Проверьте, является ли Seq палиндромом
   *
   */
  def Palindrome[A](seq: Seq[A]): Boolean = seq.reverse == seq

  def testPalindrome[A](seq: Seq[A]): Boolean = Palindrome(seq)

  println(testPalindrome(list3))
  /* e) Реализуйте flatMap используя foldLeft.
   *
   */
  def FlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft(Seq[B]())(_ ++ f(_))

  def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = FlatMap(seq)(f)

}
