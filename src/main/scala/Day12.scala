import scala.annotation.tailrec

object Day12 extends App {

  case class Position(x: Int, y: Int, z: Int) {

    def +(vel: Velocity): Position =
      Position(this.x + vel.x, this.y + vel.y, this.z + vel.z)
  }

  case class Velocity(x: Int, y: Int, z: Int)

  case class Moon(pos: Position, vel: Velocity) {

    def equalsX(other: Moon): Boolean =
      (this.pos.x == other.pos.x) && (this.vel.x == other.vel.x)

    def equalsY(other: Moon): Boolean =
      (this.pos.y == other.pos.y) && (this.vel.y == other.vel.y)

    def equalsZ(other: Moon): Boolean =
      (this.pos.z == other.pos.z) && (this.vel.z == other.vel.z)
  }

  def getUniverse(file: String) = {

    val pattern = "<x=(.+), y=(.+), z=(.+)>".r

    scala.io.Source
      .fromResource(file)
      .getLines()
      .toSeq
      .map(conf => {
        val pattern(x, y, z) = conf
        Moon(Position(x.toInt, y.toInt, z.toInt), Velocity(0, 0, 0))
      })
  }

  def updateVelocity(moon: Moon, universe: Seq[Moon]): Velocity =
    universe.foldLeft(moon.vel) { (vel: Velocity, moon2: Moon) =>
      {
        val delX =
          if (moon.pos.x < moon2.pos.x) 1
          else if (moon.pos.x > moon2.pos.x) -1
          else 0
        val delY =
          if (moon.pos.y < moon2.pos.y) 1
          else if (moon.pos.y > moon2.pos.y) -1
          else 0
        val delZ =
          if (moon.pos.z < moon2.pos.z) 1
          else if (moon.pos.z > moon2.pos.z) -1
          else 0
        Velocity(vel.x + delX, vel.y + delY, vel.z + delZ)
      }
    }

  def stepVelocity(universeMap: Seq[Moon]): Seq[Moon] =
    universeMap.map { sat =>
      Moon(sat.pos, updateVelocity(sat, universeMap diff Seq(sat)))
    }

  def stepPosition(universeMap: Seq[Moon]): Seq[Moon] =
    universeMap.map { moon => Moon(moon.pos + moon.vel, moon.vel) }

  @tailrec
  def performSteps(universeMap: Seq[Moon], count: Int): Seq[Moon] =
    if (count == 0) universeMap
    else performSteps(stepPosition(stepVelocity(universeMap)), count - 1)

  def totalEnergy(universe: Seq[Moon]): Int =
    universe.map { moon =>
      {
        ((Math.abs(moon.pos.x) + Math.abs(moon.pos.y) + Math.abs(moon.pos.z))
          * (Math.abs(moon.vel.x) + Math.abs(moon.vel.y) + Math.abs(
            moon.vel.z
          )))
      }
    }.sum

  val universe = getUniverse("inputs/day12.txt")
  val newUniverse = performSteps(universe, 1000)

  println(s"Day 12 part1 = ${totalEnergy(newUniverse)}")

  def checkRepetition(
      universeOrigin: Seq[Moon],
      universe: Seq[Moon],
      counter: Long,
      repetitions: (Long, Long, Long)
  ): (Long, Long, Long) = {

    val zippedUniverse = universeOrigin.zip(universe)
    val repX =
      if (
        (zippedUniverse.filterNot { case (a, b) =>
          a.equalsX(b)
        }.size == 0) && (repetitions._1 == 0)
      ) counter
      else repetitions._1
    val repY =
      if (
        (zippedUniverse.filterNot { case (a, b) =>
          a.equalsY(b)
        }.size == 0) && (repetitions._2 == 0)
      ) counter
      else repetitions._2
    val repZ =
      if (
        (zippedUniverse.filterNot { case (a, b) =>
          a.equalsZ(b)
        }.size == 0) && (repetitions._3 == 0)
      ) counter
      else repetitions._3

    (repX, repY, repZ)
  }

  @tailrec
  def runUniverse(
      universeOrgin: Seq[Moon],
      universe: Seq[Moon],
      counter: Long,
      repetitions: (Long, Long, Long)
  ): (Long, Long, Long) = {
    val newRepetitions =
      checkRepetition(universeOrgin, universe, counter, repetitions)
    if (
      newRepetitions._1 == 0 || newRepetitions._2 == 0 || newRepetitions._3 == 0
    )
      runUniverse(
        universeOrgin,
        stepPosition(stepVelocity(universe)),
        counter + 1,
        newRepetitions
      )
    else
      newRepetitions
  }

  @tailrec
  def gcd(a: Long, b: Long): Long =
    if (b == 0) a else gcd(b, a % b)

  println(
    s"Day 12 part2 = ${runUniverse(universe, stepPosition(stepVelocity(universe)), 1, (0, 0, 0)).productIterator.toList
      .foldLeft(1L) { (a: Long, b: Any) =>
        a / gcd(a, b.asInstanceOf[Number].longValue()) * b
          .asInstanceOf[Number]
          .longValue()
      }}"
  )
}
