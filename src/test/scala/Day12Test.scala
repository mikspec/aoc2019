import Day12.{gcd, runUniverse, stepPosition, stepVelocity, universe}

class Day12Test extends org.scalatest.FunSuite {

  test("Day 12 test part 01") {

    val universe = Day12.getUniverse("inputs/day12-02.txt")

    assert(
      Day12.totalEnergy(Day12.performSteps(universe, 100)) === 1940
    )
  }

  test("Day 12 test part 02 - 1") {

    val universe = Day12.getUniverse("inputs/day12-01.txt")

    assert(
      runUniverse(
        universe,
        stepPosition(stepVelocity(universe)),
        1,
        (0, 0, 0)
      ).productIterator.toList
        .foldLeft(1L) { (a: Long, b: Any) =>
          a / gcd(a, b.asInstanceOf[Number].longValue()) * b
            .asInstanceOf[Number]
            .longValue()
        }
        === 2772
    )
  }

  test("Day 12 test part 02 - 2") {

    val universe = Day12.getUniverse("inputs/day12-02.txt")
    val newUniverse = stepPosition(stepVelocity(universe))

    assert(
      runUniverse(
        universe,
        stepPosition(stepVelocity(universe)),
        1,
        (0, 0, 0)
      ).productIterator.toList
        .foldLeft(1L) { (a: Long, b: Any) =>
          a / gcd(a, b.asInstanceOf[Number].longValue()) * b
            .asInstanceOf[Number]
            .longValue()
        }
        === 4686774924L
    )
  }
}
