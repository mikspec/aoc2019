import scala.annotation.tailrec

object Day01 extends App {

  val modulesWeight = scala.io.Source.fromResource("inputs/day01.txt").getLines.takeWhile(_ != "").map(_.toInt).toList
  val fuel = modulesWeight.map( x => x / 3 - 2).sum

  println(s"Day01 part 1 ${fuel}")
  
  @tailrec
  def fuelCalc(x: Int, sum:Int):Int = {
    if ((x / 3) >= 2) fuelCalc(x / 3 - 2, sum + x / 3 - 2)
    else sum
  }

  var fuelModified = modulesWeight.map(x => fuelCalc(x, 0)).sum
  println(s"Day01 part 2 ${fuelModified}")

  val listNew = modulesWeight.map(m =>
    m / 3 - 2 :: LazyList.iterate(m)(_ / 3 - 2).drop(1).takeWhile(_ > 0).sum :: Nil
  )
  println(listNew.transpose.map(_.sum).zipWithIndex.map(t => s"Part ${t._2 + 1}: ${t._1}").mkString("\n"))
}