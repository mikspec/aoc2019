import scala.annotation.tailrec
import scala.util.matching.Regex

object Day16 extends App {

  val PATTERN:List[Int] = List(0, 1, 0, -1)

  def getInput(file: String):List[Int] = {
    scala.io.Source
      .fromResource(file)
      .toList
      .map(digit => digit - '0'.toInt)
  }  

  println(s"Day 16 part 1 ${getInput("inputs/day16.txt")}")
}