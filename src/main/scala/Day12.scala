import scala.annotation.tailrec

object Day12 extends App {

  val pattern = "<x=(.+), y=(.+), z=(.+)>".r

  case class Moon(posX: Int, posY: Int, posZ: Int, velX: Int, velY: Int, velZ: Int)

  val universeMap: Seq[Moon] = scala.io.Source.fromResource("inputs/day12.txt")
    .getLines()
    .toSeq.map(conf => {
        val pattern(x, y, z) = conf
        Moon(x.toInt, y.toInt, z.toInt, 0, 0, 0)
    })

  println(s"Day 12 part1 = ${universeMap}")
}