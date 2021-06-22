import Day09.{State, intProg}

import scala.annotation.tailrec

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

  object Color extends Enumeration {
    type Color = Value
    val WHITE, BLACK = Value
  }

  case class Position(x: Int, y: Int) {

    def +(direction: Direction) = Position(this.x + direction.dirX, this.y + direction.dirY)
  }

  sealed abstract class Direction(val dirX: Int, val dirY: Int)

  case object UP extends Direction(0, -1)
  case object RIGHT extends Direction(1, 0)
  case object DOWN extends Direction(0, 1)
  case object LEFT extends Direction(-1, 0)

  case class Field(color: Color.Color, painted: Boolean)

  type Surface = Map[Position, Field]

  case class Robot(position: Position, dir: Direction) {

    def paint(color: Color.Color, surface: Surface): Surface =
      surface.updated(this.position, Field(color, true))

    def turn(goRight: Int): Direction = {
      if (goRight == 1) {
        this.dir match {
          case UP => RIGHT
          case RIGHT => DOWN
          case DOWN => LEFT
          case LEFT => UP
        }
      }
      else {
        this.dir match {
          case UP => LEFT
          case LEFT => DOWN
          case DOWN => RIGHT
          case RIGHT => UP
        }
      }
    }

    def step(turnDir: Int): Robot = {
      val newDir = this.turn(turnDir)
      val newPos = this.position + newDir
      Robot(newPos, newDir)
    }

    def camera(surface: Map[Position, Field]): Color.Color =
      surface.getOrElse(this.position, Field(Color.BLACK, false)).color
  }

  @tailrec
  def runRobot(robot: Robot,
               surface: Surface,
               stack: Map[BigInt, BigInt],
               index: BigInt,
               relativeBaseOffset: BigInt): Surface = {
    val color = robot.camera(surface)
    val conf = Seq(if (color == Color.WHITE) BigInt(1) else BigInt(0))
    intProg(stack, conf, index, relativeBaseOffset) match {
      case State(output1, stack1, index1, relativeBaseOffset1, _) => {
        if (index1 == -1) surface
        else {
          val paintColor = if (output1 == 1) Color.WHITE else Color.BLACK
          val newSurface = robot.paint(paintColor, surface)
          intProg(stack1, Seq(), index1, relativeBaseOffset1) match {
            case State(output2, stack2, index2, relativeBaseOffset2, _) => {
              if (index2 == -1) newSurface
              else {
                val newRobot = robot.step(output2.toInt)
                runRobot(newRobot, newSurface, stack2, index2, relativeBaseOffset2)
              }
            }
          }
        }
      }
    }
  }

  val surface: Surface = runRobot(Robot(Position(0,0), UP), Map(), progCode, BigInt(0), BigInt(0))
  println(s"Day 11 part1 = ${surface.size}")

  val surface2 = runRobot(
    Robot(Position(0,0), UP),
    Map(Position(0,0) -> Field(Color.WHITE, false)),
    progCode,
    BigInt(0),
    BigInt(0))
    .groupBy { _._1.y }

  println(s"Day 11 part2")

  (0 to 5).foreach(y => {
    val row = surface2.getOrElse(y, Map())
    (0 to 42).foreach(x => {
      val field = row.getOrElse(Position(x, y), Field(Color.BLACK, false))
      if ( field.color == Color.WHITE) print("#")
      else print(" ")
    })
    print("\n")
  })
}
