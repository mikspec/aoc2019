import Day09.{State, intProg}
import scala.collection.immutable.Queue
import scala.annotation.tailrec

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
    val WALL, UNKNOWN, OXYGENE = Value
  }

  type Surface = Map[Position, Either[SurfaceState.SurfaceState, Int]]

  case class Params(
      oxygenSystem: Option[Position],
      dron: Position,
      surface: Surface,
      compState: State
  )

  def move(
      params: Params,
      direction: Direction,
      isForcedMove: Boolean
  ): (Params, Boolean) = {

    val newPosition = params.dron + direction

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
                newPosition,
                params.surface.updated(
                  newPosition,
                  Right(params.surface(params.dron).toOption.get + 1)
                ),
                moveOutput
              ),
              true
            )
          case OXYGEN_CODE =>
            (
              Params(
                Option(newPosition),
                newPosition,
                params.surface.updated(
                  newPosition,
                  Right(params.surface(params.dron).toOption.get + 1)
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
            .surface(params.dron)
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
              newPosition,
              params.surface.updated(
                newPosition,
                if (isForcedMove) params.surface(newPosition)
                else
                  Right(params.surface(params.dron).toOption.get + 1)
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
      dirSeq.isEmpty || (!params.oxygenSystem.isEmpty && params.dron == params.oxygenSystem.get)
    ) return params

    val moveOutput = move(params, dirSeq.head, false) match {
      case (moveState, false) => moveState
      case (moveState, true) =>
        move(runDron(moveState, DIRS), dirSeq.head.getOposite(), true)._1
    }
    runDron(moveOutput, dirSeq.tail)
  }

  def printSurface(
      surface: Surface,
      dron: Position,
      oxygenSystem: Option[Position]
  ) = {
    val xcords = surface.map { pos => pos._1.x }.toList
    val ycords = surface.map { pos => pos._1.y }.toList

    (ycords.min to ycords.max).foreach(y => {
      (xcords.min to xcords.max).foreach(x => {
        (surface.getOrElse(
          Position(x, y),
          Left(SurfaceState.UNKNOWN)
        ): @unchecked) match {
          case Left(SurfaceState.WALL)    => print('#')
          case Left(SurfaceState.UNKNOWN) => print(' ')
          case Left(SurfaceState.OXYGENE) => print('O')
          case Right(value) => {
            val field =
              if (dron == Position(x, y)) 'D'
              else if (
                !oxygenSystem.isEmpty && oxygenSystem.get == Position(
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
      Position(0, 0),
      Map(Position(0, 0) -> Right(0)),
      State(0, progCode, 0, 0, Seq())
    ),
    DIRS
  )

  printSurface(output.surface, output.dron, output.oxygenSystem)

  println(
    s"Day 15 part 1 ${output.oxygenSystem} ${output.surface.get(output.oxygenSystem.get)}"
  )

  case class Oxygene(val pos: Position, val time: Int)

  case class OxygeneParams(
      val surface: Surface,
      val oxygenePoint: Oxygene,
      val oxygeneSpread: Queue[Oxygene]
  )

  @tailrec
  def spreadOxygene(
      oxygeneParams: OxygeneParams,
      dirSeq: Seq[Direction]
  ): OxygeneParams = {
    if (dirSeq.isEmpty) oxygeneParams
    else {
      val newPosition = oxygeneParams.oxygenePoint.pos + dirSeq.head
      oxygeneParams.surface(newPosition) match {
        case Left(_) => spreadOxygene(oxygeneParams, dirSeq.tail)
        case Right(_) =>
          spreadOxygene(
            OxygeneParams(
              oxygeneParams.surface
                .updated(newPosition, Left(SurfaceState.OXYGENE)),
              oxygeneParams.oxygenePoint,
              oxygeneParams.oxygeneSpread :+ Oxygene(
                newPosition,
                oxygeneParams.oxygenePoint.time + 1
              )
            ),
            dirSeq.tail
          )
      }
    }
  }

  def runOxygene(oxygeneParams: OxygeneParams): OxygeneParams = {
    val newParams = spreadOxygene(oxygeneParams, DIRS)
    if (newParams.oxygeneSpread.isEmpty) oxygeneParams
    else {
      val (head, tail) = newParams.oxygeneSpread.dequeue
      runOxygene(OxygeneParams(newParams.surface, head, tail))
    }
  }

  var output2 = runOxygene(
    OxygeneParams(
      output.surface
        .updated(output.oxygenSystem.get, Left(SurfaceState.OXYGENE)),
      Oxygene(output.oxygenSystem.get, 0),
      Queue()
    )
  )

  printSurface(output2.surface, output.dron, output.oxygenSystem)

  println(
    s"Day 15 part 2 ${output2.oxygenePoint.time}"
  )
}
