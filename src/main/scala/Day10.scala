import scala.annotation.tailrec

object Day10 extends App {

  val image = scala.io.Source.fromResource("inputs/day10.txt").getLines().toSeq

  case class Position(rowNum: Int, colNum: Int)

  def createMap(image: Seq[String]): Set[Position] =
    image.zipWithIndex
      .map {
        case (line, rowNum) => {
          line.zipWithIndex.map { case (asteroid, colNum) =>
            (asteroid, rowNum, colNum)
          }
        }
      }
      .flatten
      .filter { case (asteroid, _, _) => asteroid == '#' }
      .map { case (_, rowNum, colNum) => Position(rowNum, colNum) }
      .toSet

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def calculateVisibility(
      asteroid: Position,
      skyMap: Set[Position]
  ): Int =
    skyMap.map {
      case Position(rowNum, colNum) => {
        val div = gcd(
          Math.abs(rowNum - asteroid.rowNum),
          Math.abs(colNum - asteroid.colNum)
        )
        ((rowNum - asteroid.rowNum) / div, (colNum - asteroid.colNum) / div)
      }
    }.size

  def processSkyMap(skyMap: Set[Position]): Tuple2[Position, Int] =
    skyMap
      .map { asteroid =>
        (asteroid, calculateVisibility(asteroid, skyMap - asteroid))
      }
      .reduce { (a, b) => if (a._2 > b._2) a else b }

  println(s"Day 10 part1 = ${processSkyMap(createMap(image))}")
}
