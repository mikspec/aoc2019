import Day09.{State, intProg}

import scala.annotation.tailrec

object Day15 extends App {

  val progCode = scala.io.Source
    .fromResource("inputs/day15.txt")
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
            stack1,
            index1,
            relativeBaseOffset1,
            output :+ output1.toInt
          )
      }
    }
  }

  println(s"Day 15 part1 ${intProg(progCode, Seq(4), 0, 0)}}")

}
