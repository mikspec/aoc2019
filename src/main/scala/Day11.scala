object Day11 extends App {

  val progCode = scala.io.Source
    .fromResource("inputs/day11.txt")
    .getLines()
    .toSeq
    .flatMap(_.split(","))
    .map { x => BigInt(x.toLong) }
    .zipWithIndex
    .map { case (value, index) => BigInt(index) -> value }
    .toMap

  case class Position(x: Int, y: Int)

  case class Direction(dirX: Int, dirY: Int)

  case class Field(position: Position, color: Boolean, painted: Boolean)

  case class Robot(position: Position, dir: Direction)

  val state1 = Day09.intProg(progCode, Seq(0), 0, 0)
  println(s"Day 11 part1 = ${state1}")
  val state2 = Day09.intProg(state1.stack, Seq(), state1.index, state1.relativeBaseOffset)
  println(s"Day 11 part2 = ${state2}")
}
