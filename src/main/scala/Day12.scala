import scala.annotation.tailrec

object Day12 extends App {

  case class Position(x: Int, y: Int, z: Int) {

    def +(vel: Velocity): Position =
      Position(this.x + vel.x, this.y + vel.y, this.z + vel.z)
  }

  case class Velocity(x: Int, y: Int, z: Int)

  case class Moon(pos: Position, vel: Velocity)

  def getUniverse(file: String) = {

    val pattern = "<x=(.+), y=(.+), z=(.+)>".r

    scala.io.Source.fromResource(file)
      .getLines()
      .toSeq.map(conf => {
      val pattern(x, y, z) = conf
      Moon(Position(x.toInt, y.toInt, z.toInt), Velocity(0, 0, 0))
    })
  }

  def updateVelocity(moon: Moon, universe: Seq[Moon]): Velocity =
    universe.foldLeft(moon.vel) {
      (vel: Velocity, moon2: Moon) => {
        val delX = if (moon.pos.x < moon2.pos.x) 1 else if (moon.pos.x > moon2.pos.x) -1 else 0
        val delY = if (moon.pos.y < moon2.pos.y) 1 else if (moon.pos.y > moon2.pos.y) -1 else 0
        val delZ = if (moon.pos.z < moon2.pos.z) 1 else if (moon.pos.z > moon2.pos.z) -1 else 0
        Velocity(vel.x + delX, vel.y + delY, vel.z + delZ)
      }
    }

  def stepVelocity(universeMap: Seq[Moon]): Seq[Moon] =
    universeMap.map {
      sat => Moon(sat.pos, updateVelocity(sat, universeMap diff Seq(sat)))
    }

  def stepPosition(universeMap: Seq[Moon]): Seq[Moon] =
    universeMap.map { moon => Moon(moon.pos + moon.vel, moon.vel) }

  @tailrec
  def performSteps(universeMap: Seq[Moon], count: Int): Seq[Moon] =
    if (count == 0) universeMap
    else performSteps(stepPosition(stepVelocity(universeMap)), count - 1)

  def totalEnergy(universe: Seq[Moon]): Int =
    universe.map {
      moon => {
        ((Math.abs(moon.pos.x) + Math.abs(moon.pos.y) + Math.abs(moon.pos.z))
        * (Math.abs(moon.vel.x) + Math.abs(moon.vel.y) + Math.abs(moon.vel.z)))
      }
    }
    .sum

  val universe = getUniverse("inputs/day12.txt")
  val newUniverse = performSteps(universe, 1000)

  println(s"Day 12 part1 = ${totalEnergy(newUniverse)}")

  @tailrec
  def checkRepetition(universeOrigin: Seq[Moon],
                     universe: Seq[Moon],
                     counter: Long,
                     repetitions: Map[Moon, (Long, Long, Long)]
                    ):Map[Moon, (Long, Long, Long)] = {

    def compareMoon(moonOrigin: Moon,
                    moonNew: Moon,
                    repetitions: Map[Moon, (Long, Long, Long)],
                    counter: Long
                   ): Map[Moon, (Long, Long, Long)] = {
      val repForMoon = repetitions.getOrElse(moonOrigin, (0L,0L,0L))
      val repX: Long = if (moonNew.pos.x == moonOrigin.pos.x && moonNew.vel.x == 0 && repForMoon._1 == 0) counter else repForMoon._1
      val repY: Long = if (moonNew.pos.y == moonOrigin.pos.y && moonNew.vel.y == 0 && repForMoon._2 == 0) counter else repForMoon._2
      val repZ: Long = if (moonNew.pos.z == moonOrigin.pos.z && moonNew.vel.z == 0 && repForMoon._3 == 0) counter else repForMoon._3
      repetitions.updated(moonOrigin, (repX, repY, repZ))
    }

    if (universeOrigin.isEmpty) repetitions
    else {
      val newRepetitions = compareMoon(universeOrigin.head, universe.head, repetitions, counter)
      checkRepetition(universeOrigin.tail, universe.tail, counter, newRepetitions)
    }
  }

  @tailrec
  def runUniverse(universeOrgin: Seq[Moon],
                  universe: Seq[Moon],
                  counter: Long,
                  repetitions: Map[Moon, (Long, Long, Long)]
                 ): Map[Moon, (Long, Long, Long)] = {
    val newRepetitions = checkRepetition(universeOrgin, universe, counter, repetitions)
    if (newRepetitions.find { case (_ -> v) => ((v._1 == 0) || (v._2 == 0) || (v._3 == 0)) }.nonEmpty)
      runUniverse(universeOrgin, stepPosition(stepVelocity(universe)), counter + 1, newRepetitions)
    else
      newRepetitions
  }

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == 0) a else gcd(b, a % b)

  println(s"Day 12 part2 = ${runUniverse(universe, stepPosition(stepVelocity(universe)), 1, Map())
    .flatMap { case (_, v) => List(v._1, v._2, v._3) }
    .foldLeft(BigInt(1)) { (a: BigInt, b: Long) => a / gcd(a, BigInt(b)) * b }
  }")
}

