import Day09.{State, intProg}
import scala.annotation.tailrec

object Day17 extends App {

  val SCAFFOLD = 35
  val SPACE = 46
  val NEWLINE = 10

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
      (xcords.min to xcords.max).foreach(x => {
        print(surface.getOrElse(Position(x, y), 46).toChar)
      })
      print("\n")
    })
    print("\n")
  }

  def readCamera(
      x: Int,
      y: Int,
      surface: Surface,
      progCode: Map[BigInt, BigInt],
      offset: BigInt,
      baseOffset: BigInt
  ): Surface = {
    intProg(progCode, Seq(), offset, baseOffset) match {
      case State(output, stack, index, relativeBaseOffset) => {
        if (index == -1) surface
        else {
          output.toInt match {
            case NEWLINE =>
              readCamera(0, y + 1, surface, stack, index, relativeBaseOffset)
            case default =>
              readCamera(
                x + 1,
                y,
                surface.updated(Position(x, y), output.toInt),
                stack,
                index,
                relativeBaseOffset
              )
          }
        }
      }
    }
  }

  def isCross(surface: Surface, position: Position): Boolean =
    (surface.getOrElse(Position(position.x, position.y - 1), SPACE) == SCAFFOLD) &&
      (surface.getOrElse(Position(position.x, position.y + 1), SPACE) == SCAFFOLD) &&
      (surface.getOrElse(Position(position.x - 1, position.y), SPACE) == SCAFFOLD) &&
      (surface.getOrElse(Position(position.x + 1, position.y), SPACE) == SCAFFOLD)

  def findCrosses(surface: Surface) = {
    surface.filter({ case (position, value) =>
      value == SCAFFOLD && isCross(surface, position)
    }).toList.foldLeft(0)((sum, point) => sum + point._1.x * point._1.y)
  }

  var surface = readCamera(0, 0, Map(), progCode, 0, 0)

  printSurface(surface)

  println(
    s"Day 15 part 1 ${findCrosses(surface)}"
  )
}
