
object Functions extends App {

  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */

def Circle(r: Double):Double=Math.pow(r,2)*Math.PI

  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = Circle(r)
println(testCircle(2))


  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */
  def Area = (a:Double)=>(b:Double)=>a*b



  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = Area(a)(b)
  println(testRectangleCurried(2,3))
  // c) Напишите не карированную функцию для расчета площади прямоугольника.

def Area2(a:Double,b:Double)=a*b

  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = Area2(a,b)
  println(testRectangleUc(2,3))
}
