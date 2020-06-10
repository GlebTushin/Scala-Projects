
import scala.util.{Try, Failure, Success}


object Adts extends App{


  // a) Дан List[Int], верните элемент с индексом n
  def GetNth(list: List[Int], n: Int): Option[Int] = Some(list(n))

  // примените функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testGetNth(list: List[Int], n: Int): Option[Int] = GetNth(list, n)

  val list = List(1,2,3)
  println(testGetNth(list,  2))
  // b) Напишите функцию, увеличивающую число в два раза.

def Double(n: Option[Int]): Option[Int] = if (n.isDefined) Some (n.get*2) else None

  // примените функцию из пункта (b) здесь, не изменяйте сигнатуру
  def testDouble(n: Option[Int]): Option[Int] = Double(n)

  println(testDouble(Option(5)))

  // c) Напишите функцию, проверяющую является ли число типа Int четным. Если так, верните Right. В противном случае, верните Left("Нечетное число.").

def IsEven(n: Int): Either[String, Int] = if (n%2==0) Right(n) else Left("Not even")

  // примените функцию из пункта (c) здесь, не изменяйте сигнатуру
  def testIsEven(n: Int): Either[String, Int] = IsEven(n)

  println(testIsEven(6))
  // d) Напишите функцию, реализующую безопасное деление целых чисел. Верните Right с результатом или Left("Вы не можете делить на ноль.").

def SafeDivide(a: Int, b: Int): Either[String, Int] = if (b==0) Left("You can't divide by zero") else Right(a/b)

  // примените функцию из пункта (d) здесь, не изменяйте сигнатуру
  def testSafeDivide(a: Int, b: Int): Either[String, Int] = SafeDivide(a,b)

  println(testSafeDivide(10,2))
  // e) Обработайте исключения функции с побочным эффектом вернув 0.

def GoodOldJava(impure: String => Int, str: String): Try[Int] = Try(impure(str))

  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testGoodOldJava(impure: String => Int, str: String): Try[Int] = GoodOldJava(impure, str)
}
