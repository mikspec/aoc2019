import Day09.{State, intProg}

import scala.annotation.tailrec

object Day13 extends App {

  val progCode = scala.io.Source
    .fromResource("inputs/day13.txt")
    .getLines()
    .toSeq
    .flatMap(_.split(","))
    .map { x => BigInt(x.toLong) }
    .zipWithIndex
    .map { case (value, index) => BigInt(index) -> value }
    .toMap

  case class Position(x: Int, y: Int)

  type Surface = Map[Position, Char]

  @tailrec
  def runProgram(
      surface: Surface,
      stack: Map[BigInt, BigInt],
      index: BigInt,
      relativeBaseOffset: BigInt,
      output: Seq[Int]
  ): Seq[Int] = {
    intProg(stack, Seq(), index, relativeBaseOffset) match {
      case State(output1, stack1, index1, relativeBaseOffset1) => {
        if (index1 == -1) output
        else
          runProgram(
            surface,
            stack1,
            index1,
            relativeBaseOffset1,
            output :+ output1.toInt
          )
      }
    }
  }

  val arcadeMap = runProgram(Map(), progCode, 0, 0, Seq())
    .grouped(3)
    .toList
    .map { x =>
      val field = x.toArray
      Position(field(0), field(1)) -> field(2)
    }
    .toMap

  val maxX = arcadeMap.map { field => field._1.x }.max
  val maxY = arcadeMap.map { field => field._1.y }.max

  (0 to maxY).foreach(y => {
    (0 to maxX).foreach(x => {
      val field = arcadeMap.getOrElse(Position(x, y), 0)
      print(field)
    })
    print("\n")
  })

  println
  println(s"Day 13 part1 ${arcadeMap.filter { case (_ -> v) => v == 2 }.size}")
}
