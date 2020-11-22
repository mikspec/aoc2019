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
      runUniverse(universe, stepPosition(stepVelocity(universe)), 1, Map())
        .flatMap { case (_, v) => List(v._1, v._2, v._3) }
        .foldLeft(BigInt(1)) { (a: BigInt, b:Long) => a / gcd(a, BigInt(b)) * b }
        === 2772
    )
  }

  test("Day 12 test part 02 - 2") {

    val universe = Day12.getUniverse("inputs/day12-02.txt")

    assert(
      runUniverse(universe, stepPosition(stepVelocity(universe)), 1, Map())
        .flatMap { case (_, v) => List(v._1, v._2, v._3) }
        //foldLeft(BigInt(1)) { (a: BigInt, b:Long) => a / gcd(a, BigInt(b)) * BigInt(b) }
        /// 4686774924L
        === 2772
    )
  }
}
