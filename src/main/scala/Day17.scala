import Day09.{State, intProg}
import scala.annotation.tailrec

object Day17 extends App {

  val progCode = scala.io.Source
    .fromResource("inputs/day17.txt")
    .getLines()
    .toSeq
    .flatMap(_.split(","))
    .map { x => BigInt(x.toLong) }
    .zipWithIndex
    .map { case (value, index) => BigInt(index) -> value }
    .toMap

  case class Position(x: Int, y: Int)

  type Surface = Map[Position, Int]

  def printSurface(surface: Surface) = {
    val xcords = surface.map { pos => pos._1.x }.toList
    val ycords = surface.map { pos => pos._1.y }.toList

    (ycords.min to ycords.max).foreach(y => {
      (xcords.min to xcords.max).foreach(x => {})
      print("\n")
    })
    print("\n")
  }

  def readCamera(x: Int, y: Int, surface: Surface,
               stack: Map[BigInt, BigInt],
               index: BigInt,
               relativeBaseOffset: BigInt) = {
    intProg(stack, Seq(), index, relativeBaseOffset)
  }

  println(
    s"Day 15 part 1 ${readCamera(0, 0, Map(), progCode, 0, 0).output}"
  )
}
