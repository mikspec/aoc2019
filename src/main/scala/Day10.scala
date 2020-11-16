import scala.annotation.tailrec

object Day10 extends App {

  val image = scala.io.Source.fromResource("inputs/day10.txt").getLines().toSeq

  def createMap(image: Seq[String]): Set[Tuple2[Int, Int]] = {
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
      .map { case (_, rowNum, colNum) => (rowNum, colNum) }
      .toSet
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def calculateVisibility(
      asteroid: Tuple2[Int, Int],
      skyMap: Set[Tuple2[Int, Int]]
  ): Int =
    skyMap.map {
      case (y, x) => {
        val div = gcd(Math.abs(y - asteroid._1), Math.abs(x - asteroid._2))
        ((y - asteroid._1) / div, (x - asteroid._2) / div)
      }
    }.size

  val skyMap = createMap(image)
  val newMap = skyMap
    .map { asteroid =>
      (
        asteroid._1,
        asteroid._2,
        calculateVisibility(asteroid, skyMap - asteroid)
      )
    }
    .reduce { (a: Tuple3[Int, Int, Int], b: Tuple3[Int, Int, Int]) =>
       { if (a._3 > b._3) a else b }
    }
  println(s"Day 10 part1")
  println(newMap._3)
}
