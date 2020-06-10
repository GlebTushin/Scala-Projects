object Strings extends App{

  /* a) Преобразуйте все символы типа Char в верхний регистр (не используйте заглавные буквы).
   *
   */
  def testUppercase(str: String): String = str.toUpperCase

  println(testUppercase("damn"))
  /* b) Вставьте следующие значения в строку:
   *       Hi my name is <name> and I am <age> years old.
   *
   */
  def testInterpolations(name: String, age: Int): String = s"Hi, my name is $name and I am $age years old."

  println(testInterpolations("Mark", 20))
  /* c) Добавьте два числа в следующую строку:
   *       Hi,
   *       now follows a quite hard calculation. We try to add:
   *         a := <value of a>
   *         b := <value of b>
   *
   *         result is <a + b>
   *
   *
   */
  def testComputation(a: Int, b: Int): String = "Hi,\n" +
    "now follows a quite hard calculation. We try to add:\n" +
    s"  a := $a\n" +
    s"  b := $b\n\n" +
    s"  result is $a + $b"

  println(testComputation(5, 2))
  /* d) Если длина строки равна 2, верните всю строку, иначе верните первые два символа строки.
   */
  def testTakeTwo(str: String): String = str.length match {
    case 2 => str
    case _ => str.substring(0,2)
  }
  println(testTakeTwo("string"))
}
