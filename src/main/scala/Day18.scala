object Day18 extends App {

  case class Position(x: Int, y: Int)
  case class Field(value: Char, distance: Option[Int])

  val DOORS = '@'
  val WALL = '#'
  val SPACE = '.'

  type Surface = Map[Position, Field]

  def getInput(file: String): Surface = scala.io.Source
    .fromResource(file)
    .getLines()
    .toSeq
    .map { _.toVector.zipWithIndex }
    .zipWithIndex
    .flatMap { line =>
      line._1.map { pos =>
        (Position(pos._2, line._2), Field(pos._1, Option.empty))
      }
    }
    .toMap

  val surface = getInput("inputs/day18-01.txt")
  val doors = surface.filter { _._2.value == DOORS }
  val numberOfKeys = surface.filter(_._2.value.isLower).size

  def scanSurface(surface:Surface) = ???

  println(
    s"Day 18 part 1 ${numberOfKeys}"
  )
}
