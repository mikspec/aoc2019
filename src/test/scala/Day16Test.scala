import Day16.{getInput, runPhase, generatePattern, runNumberOfPhases}

class Day16Test extends org.scalatest.FunSuite {

  test("Day 16 test part 01 - case 01") {

    val input = getInput("inputs/day16-01.txt")
    assert(
      runPhase(input, 0, List()) === List(4, 8, 2, 2, 6, 1, 5, 8)
    )
  }

  test("Day 16 test part 01 - case 01 - 4 phases") {

    val input = getInput("inputs/day16-01.txt")
    assert(
      runNumberOfPhases(input, 4) === List(0, 1, 0, 2, 9, 4, 9, 8)
    )
  }

  test("Day 16 test part 01 - case 02 - 100 phases") {

    val input = getInput("inputs/day16-02.txt")
    assert(
      runNumberOfPhases(input, 100).take(8) === List(2, 4, 1, 7, 6, 1, 7, 6)
    )
  }

}
