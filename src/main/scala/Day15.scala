import Day09.{State, intProg}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day15 extends App {

  val WALL_CODE = BigInt(0)
  val MOVED_CODE = BigInt(1)
  val OXYGEN_CODE = BigInt(2)

  val progCode = scala.io.Source
    .fromResource("inputs/day15.txt")
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
    def getOposite(): Direction
  }

  case object NORTH extends Direction(0, -1, 1) {
    def getOposite(): Direction = SOUTH
  }
  case object SOUTH extends Direction(0, 1, 2) {
    def getOposite(): Direction = NORTH
  }
  case object WEST extends Direction(-1, 0, 3) {
    def getOposite(): Direction = EAST
  }
  case object EAST extends Direction(1, 0, 4) {
    def getOposite(): Direction = WEST
  }

  val DIRS = Seq(NORTH, EAST, SOUTH, WEST)

  object SurfaceState extends Enumeration {
    type SurfaceState = Value
    val WALL, UNKNOWN = Value
  }

  type Surface = Map[Position, Either[SurfaceState.SurfaceState, Int]]

  case class Dron(position: Position, path: Queue[Direction])

  case class Params(
      oxygenSystem: Option[Position],
      dron: Dron,
      surface: Surface,
      compState: State
  )

  def move(
      params: Params,
      direction: Direction,
      isForcedMove: Boolean
  ): (Params, Boolean) = {

    val newPosition = params.dron.position + direction

    (params.surface.getOrElse(
      newPosition,
      Left(SurfaceState.UNKNOWN)
    ): @unchecked) match {
      case Left(SurfaceState.UNKNOWN) => {
        val moveOutput = intProg(
          params.compState.stack,
          Seq(direction.code),
          params.compState.index,
          params.compState.relativeBaseOffset
        )
        moveOutput.output match {
          case WALL_CODE =>
            (
              Params(
                params.oxygenSystem,
                params.dron,
                params.surface.updated(newPosition, Left(SurfaceState.WALL)),
                moveOutput
              ),
              false
            )
          case MOVED_CODE =>
            (
              Params(
                params.oxygenSystem,
                Dron(newPosition, params.dron.path :+ direction),
                params.surface.updated(
                  newPosition,
                  Right(params.surface(params.dron.position).toOption.get + 1)
                ),
                moveOutput
              ),
              true
            )
          case OXYGEN_CODE =>
            (
              Params(
                Option(newPosition),
                Dron(newPosition, params.dron.path :+ direction),
                params.surface.updated(
                  newPosition,
                  Right(params.surface(params.dron.position).toOption.get + 1)
                ),
                moveOutput
              ),
              true
            )
        }
      }
      case Left(SurfaceState.WALL) =>
        (params, false)
      case Right(pathLength) => {
        if (
          (pathLength > (params
            .surface(params.dron.position)
            .toOption
            .get + 1)) || isForcedMove
        ) {
          val moveOutput = intProg(
            params.compState.stack,
            Seq(direction.code),
            params.compState.index,
            params.compState.relativeBaseOffset
          )
          (
            Params(
              params.oxygenSystem,
              Dron(newPosition, params.dron.path :+ direction),
              params.surface.updated(
                newPosition,
                if (isForcedMove) params.surface(newPosition)
                else
                  Right(params.surface(params.dron.position).toOption.get + 1)
              ),
              moveOutput
            ),
            true
          )
        } else (params, false)
      }
    }
  }

  def runDron(params: Params, dirSeq: Seq[Direction]): Params = {
    if (
      dirSeq.isEmpty || (!params.oxygenSystem.isEmpty && params.dron.position == params.oxygenSystem.get)
    ) return params

    val moveOutput = move(params, dirSeq.head, false) match {
      case (moveState, false) => moveState
      case (moveState, true) =>
        move(runDron(moveState, DIRS), dirSeq.head.getOposite(), true)._1
    }
    runDron(moveOutput, dirSeq.tail)
  }

  def printSurface(params: Params) = {
    val xcords = params.surface.map { pos => pos._1.x }.toList
    val ycords = params.surface.map { pos => pos._1.y }.toList

    (ycords.min to ycords.max).foreach(y => {
      (xcords.min to xcords.max).foreach(x => {
        (params.surface.getOrElse(
          Position(x, y),
          Left(SurfaceState.UNKNOWN)
        ): @unchecked) match {
          case Left(SurfaceState.WALL)    => print('#')
          case Left(SurfaceState.UNKNOWN) => print(' ')
          case Right(value) => {
            val field =
              if (params.dron.position == Position(x, y)) 'D'
              else if (
                !params.oxygenSystem.isEmpty && params.oxygenSystem.get == Position(
                  x,
                  y
                )
              ) 'O'
              else '.'
            print(field)
          }
        }
      })
      print("\n")
    })
    print("\n")
  }

  var output = runDron(
    Params(
      Option.empty,
      Dron(Position(0, 0), Queue()),
      Map(Position(0, 0) -> Right(0)),
      State(0, progCode, 0, 0)
    ),
    DIRS
  )

  printSurface(output)

  println(s"Day 15 part1 ${output.oxygenSystem} ${output.surface.get(output.oxygenSystem.get)}")
}
