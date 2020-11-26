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

  val arcadeMap = runProgram(progCode, 0, 0, Seq())
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

  @tailrec
  def playGame(
      stack: Map[BigInt, BigInt],
      index: BigInt,
      relativeBaseOffset: BigInt,
      output: Seq[Int],
      input: Seq[BigInt],
      ball: Position,
      paddle: Position,
      score: BigInt
  ): BigInt = {
    intProg(stack, input, index, relativeBaseOffset) match {
      case State(output1, stack1, index1, relativeBaseOffset1) => {
        if (index1 == -1) score
        else {
          val newOutput =
            if (output.size == 2) Seq() else output :+ output1.toInt
          val newScore =
            if (output.size == 2 && output.head == -1 && output.tail.head == 0)
              output1
            else score
          val newBall =
            if (output.size == 2 && output1 == BigInt(4))
              Position(output.head, output.tail.head)
            else ball
          val newPaddle =
            if (output.size == 2 && output1 == BigInt(3))
              Position(output.head, output.tail.head)
            else paddle
          val joystic = if (newBall != null && newPaddle != null) {
            if (newBall.x == newPaddle.x) Seq(BigInt(0))
            else if (newBall.x > newPaddle.x) Seq(BigInt(1))
            else Seq(BigInt(-1))
          } else Seq(BigInt(0))
          playGame(
            stack1,
            index1,
            relativeBaseOffset1,
            newOutput,
            joystic,
            newBall,
            newPaddle,
            newScore
          )
        }
      }
    }
  }

  println(
    s"Day 13 part2 ${playGame(progCode.updated(0, 2), 0, 0, Seq(), Seq(), null, null, 0)}"
  )
}
