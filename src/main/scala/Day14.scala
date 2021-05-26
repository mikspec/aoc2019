import scala.annotation.tailrec

object Day14 extends App {

  val recipe = scala.io.Source
    .fromResource("inputs/day14.txt")
    .getLines()
    .toSeq

  println(
    s"Day 13 part2 ${recipe}"
  )
}
