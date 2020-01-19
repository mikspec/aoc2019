import scala.annotation.tailrec

object Day06 extends App {

  val UniverseCenter = "COM"
  val You = "YOU"
  val San = "SAN"
  val pattern = "(.+)\\)(.+)".r

  val universeMap = scala.io.Source.fromResource("inputs/day06.txt")
    .getLines()
    .toSeq.map(conf => {
        val pattern(center,satellite) = conf
        (center, satellite) 
    })
    .groupBy { case (center, satellite) => center }
    .map { case (k, list) => k -> list.map { _._2 } }
    
  def distanceMap(universe: Map[String, Seq[String]], center: String): Map[String, Int] = {    
    @tailrec
    def traverseDist(planetList: Seq[(String, Int)], accum: Map[String, Int]): Map[String, Int] = {
      if (planetList.isEmpty) accum
      else traverseDist(
        planetList.tail ++ universe.getOrElse(planetList.head._1, Seq()).map { (_, (planetList.head._2 + 1)) }, 
        accum ++ universe.getOrElse(planetList.head._1, Seq()).map { _ -> (planetList.head._2 + 1) } 
      )
    }
    traverseDist(Seq((center, 0)), Map(center -> 0))
  }

  val distMap = distanceMap(universeMap, UniverseCenter)

  val reverseMap = universeMap.flatMap { case (center, satellite) => satellite.map { sat => sat -> center } }

  @tailrec
  def getPath(revMap: Map[String, String], from: String, path: Set[String]): Set[String] = {
    if (!revMap.isDefinedAt(from)) path
    else getPath(revMap, revMap(from), path + revMap(from))
  }

  def getPathBeeten(start: String, end: String, revMap: Map[String, String]): Int = {
    val startPath = getPath(revMap, start, Set())
    val endPath = getPath(revMap, end, Set())
    (startPath.diff(endPath) ++ endPath.diff(startPath)).size
  }

  println(s"Day 06 part1 = ${distMap.foldLeft(0) { (sum, planet) => sum + planet._2 }}")
  println(s"Day 06 part2 = ${getPathBeeten(You, San, reverseMap)}")
}