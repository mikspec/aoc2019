import scala.annotation.tailrec
import javax.security.sasl.RealmCallback
import scala.collection.immutable.ListMap

object Day10 extends App {

  val image =
    scala.io.Source.fromResource("inputs/day10.txt").getLines().toSeq

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

  val skyMap = createMap(image)
  val station = processSkyMap(skyMap)

  println(s"Day 10 part1 = ${station}")

  def calcAngle(
      stationPos: Position,
      asteroid: Position
  ): Tuple3[Double, Int, Position] = {
    val x = asteroid.colNum - stationPos.colNum
    val y = asteroid.rowNum - stationPos.rowNum
    val d = Math.abs(x) + Math.abs(y)
    val angle =
      if (x >= 0 && y < 0) x.toDouble / d
      else if (x >= 0 && y >= 0) 2 - x.toDouble / d
      else if (x < 0 && y >= 0) 2 + Math.abs(x.toDouble) / d
      else 4 - Math.abs(x.toDouble) / d

    (angle, d, asteroid)
  }

  def laserMap(
      stationPos: Position,
      skyMap: List[Position]
  ): ListMap[Double, List[Tuple3[Double, Int, Position]]] =
    ListMap(
      skyMap
        .map { asteroid => calcAngle(stationPos, asteroid) }
        .groupBy { case (angle, _, _) => angle }
        .toSeq
        .sortBy(_._1): _*
    )
      .map { case (k, v) =>
        (k, v.sortBy { case (_, distance, _) => distance })
      }

  @tailrec
  def laserRun(
      skyMap: ListMap[Double, List[Tuple3[Double, Int, Position]]],
      lastVaporized: Position,
      count: Int
  ): Position = {
    if (count == 0) lastVaporized
    else if (skyMap.head._2.size == 1)
      laserRun(skyMap.tail, skyMap.head._2.head._3, count - 1)
    else
      laserRun(
        skyMap.tail + ((skyMap.head._1, skyMap.head._2.tail)),
        skyMap.head._2.head._3,
        count - 1
      )
  }

  val lastVaporized =
    laserRun(laserMap(station._1, (skyMap - station._1).toList), null, 200)
  println(s"Day 10 part 2 ${lastVaporized.colNum * 100 + lastVaporized.rowNum}")
}
