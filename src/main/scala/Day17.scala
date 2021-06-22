import Day09.{State, intProg}
import scala.annotation.tailrec

object Day17 extends App {

  val SCAFFOLD = '#'.toInt
  val SPACE = '.'.toInt
  val NEWLINE = 10
  val LEFT = 'L'.toInt
  val RIGHT = 'R'.toInt

  val progCode = scala.io.Source
    .fromResource("inputs/day17.txt")
    .getLines()
    .toSeq
    .flatMap(_.split(","))
    .map { x => BigInt(x.toLong) }
    .zipWithIndex
    .map { case (value, index) => BigInt(index) -> value }
    .toMap

  case class Position(x: Int, y: Int) {

    def +(direction: Direction) =
      Position(this.x + direction.dirX, this.y + direction.dirY)
  }

  sealed abstract class Direction(val dirX: Int, val dirY: Int, val code: Int) {
    def getLeft(): Direction
    def getRight(): Direction
  }

  case object NORTH extends Direction(0, -1, '^'.toInt) {
    def getLeft(): Direction = WEST
    def getRight(): Direction = EAST
  }
  case object SOUTH extends Direction(0, 1, 'v'.toInt) {
    def getLeft(): Direction = EAST
    def getRight(): Direction = WEST
  }
  case object WEST extends Direction(-1, 0, '<'.toInt) {
    def getLeft(): Direction = SOUTH
    def getRight(): Direction = NORTH
  }
  case object EAST extends Direction(1, 0, '>'.toInt) {
    def getLeft(): Direction = NORTH
    def getRight(): Direction = SOUTH
  }

  val DIRS = Seq(NORTH, EAST, SOUTH, WEST)
  val DIRS_MAP = DIRS.map { dir => dir.code -> dir }.toMap

  type Surface = Map[Position, Int]

  case class Vacuum(position: Position, direction: Direction)

  def printSurface(surface: Surface, vacuum: Option[Vacuum]) = {
    val xcords = surface.map { pos => pos._1.x }.toList
    val ycords = surface.map { pos => pos._1.y }.toList

    (ycords.min to ycords.max).foreach(y => {
      (xcords.min to xcords.max).foreach(x => {
        var field = surface.getOrElse(Position(x, y), SPACE).toChar
        if (vacuum.isDefined && vacuum.get.position == Position(x, y))
          field = vacuum.get.direction.code.toChar
        print(field)
      })
      print("\n")
    })
    print("\n")
  }

  def readCamera(
      x: Int,
      y: Int,
      surface: Surface,
      vacuum: Option[Vacuum],
      dirsMap: Map[Int, Direction],
      progCode: Map[BigInt, BigInt],
      offset: BigInt,
      baseOffset: BigInt
  ): Tuple2[Surface, Option[Vacuum]] = {
    intProg(progCode, Seq(), offset, baseOffset) match {
      case State(output, stack, index, relativeBaseOffset, _) => {
        if (index == -1) (surface, vacuum)
        else {
          output.toInt match {
            case NEWLINE =>
              readCamera(
                0,
                y + 1,
                surface,
                vacuum,
                dirsMap,
                stack,
                index,
                relativeBaseOffset
              )
            case default =>
              readCamera(
                x + 1,
                y,
                surface.updated(Position(x, y), output.toInt),
                if (dirsMap.get(output.toInt).isDefined)
                  Option(Vacuum(Position(x, y), dirsMap(output.toInt)))
                else vacuum,
                dirsMap,
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
    (surface.getOrElse(
      Position(position.x, position.y - 1),
      SPACE
    ) == SCAFFOLD) &&
      (surface.getOrElse(
        Position(position.x, position.y + 1),
        SPACE
      ) == SCAFFOLD) &&
      (surface.getOrElse(
        Position(position.x - 1, position.y),
        SPACE
      ) == SCAFFOLD) &&
      (surface.getOrElse(
        Position(position.x + 1, position.y),
        SPACE
      ) == SCAFFOLD)

  def findCrosses(surface: Surface): Int = {
    surface
      .filter({ case (position, value) =>
        value == SCAFFOLD && isCross(surface, position)
      })
      .toList
      .foldLeft(0)((sum, point) => sum + point._1.x * point._1.y)
  }

  val (surface, vacuum) =
    readCamera(0, 0, Map(), Option.empty, DIRS_MAP, progCode, 0, 0)

  printSurface(surface, vacuum)

  println(
    s"Day 17 part 1 ${findCrosses(surface)}"
  )

  def getNextMove(
      surface: Surface,
      vacuum: Vacuum
  ): Tuple2[Vacuum, Option[Int]] = {
    if (
      surface.getOrElse(vacuum.position + vacuum.direction, SPACE) == SCAFFOLD
    )
      (
        Vacuum(vacuum.position + vacuum.direction, vacuum.direction),
        Option.empty
      )
    else if (
      surface.getOrElse(
        vacuum.position + vacuum.direction.getRight(),
        SPACE
      ) == SCAFFOLD
    ) (Vacuum(vacuum.position, vacuum.direction.getRight()), Option(RIGHT))
    else if (
      surface.getOrElse(
        vacuum.position + vacuum.direction.getLeft(),
        SPACE
      ) == SCAFFOLD
    ) (Vacuum(vacuum.position, vacuum.direction.getLeft()), Option(LEFT))
    else (vacuum, Option.empty)
  }

  def followPath(
      surface: Surface,
      vacuum: Vacuum,
      moves: Seq[Either[Char, Int]],
      counter: Int
  ): Seq[Either[Char, Int]] = {
    val (newVacuum, newDir) = getNextMove(surface, vacuum)
    if (newVacuum == vacuum) moves :+ Right(counter)
    else {
      (newDir: @unchecked) match {
        case Some(RIGHT) =>
          followPath(
            surface,
            newVacuum,
            moves :+ Right(counter) :+ Left(RIGHT.toChar),
            0
          )
        case Some(LEFT) =>
          followPath(
            surface,
            newVacuum,
            moves :+ Right(counter) :+ Left(LEFT.toChar),
            0
          )
        case None => followPath(surface, newVacuum, moves, counter + 1)
      }
    }
  }

  println(
    s"Day 17 path ${followPath(surface, vacuum.get, Seq(), 0)
      .filter { item => item != Right(0) }
      .map(item =>
        item match {
          case Right(value) => value.toString
          case Left(value)  => value.toString
        }
      )
      .mkString(",")}"
  )

  val routine = "A,B,A,C,B,C,A,B,A,C\nR,10,L,8,R,10,R,4\nL,6,L,6,R,10\nL,6,R,12,R,12,R,10\nn\n"

  @tailrec
  def runPart2(
      progCode: Map[BigInt, BigInt],
      routine: Seq[BigInt],
      out: Seq[BigInt],
      offset: BigInt,
      baseOffset: BigInt
  ): Seq[BigInt] = {
    intProg(progCode, routine.map(_.toInt), offset, baseOffset) match {
      case State(output, stack, index, relativeBaseOffset, input) => {
        if (index == -1) out
        else runPart2(stack, input, Seq(output), index, relativeBaseOffset)
      }
    }
  }

  println(
    s"Day 17 part 2 ${runPart2(progCode.updated(0, 2), routine.map(_.toInt), Seq(), 0, 0)}"
  )
}
