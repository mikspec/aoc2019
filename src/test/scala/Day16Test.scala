import Day16.{
  getInput,
  runPhase,
  generatePattern,
  runNumberOfPhases,
  runNumberOfPhasesPart2
}

class Day16Test extends org.scalatest.FunSuite {

  test("Day 16 test part 01 - case 01") {

    val input = getInput("inputs/day16-01.txt")
    val patternMap: Map[Int, List[Int]] = (1 to input.size).toList.map { x =>
      (x, generatePattern(x, input.size).toList)
    }.toMap
    assert(
      runPhase(input, patternMap, 0, List()) === List(4, 8, 2, 2, 6, 1, 5, 8)
    )
  }

  test("Day 16 test part 01 - case 01 - 4 phases") {

    val input = getInput("inputs/day16-01.txt")
    val patternMap: Map[Int, List[Int]] = (1 to input.size).toList.map { x =>
      (x, generatePattern(x, input.size).toList)
    }.toMap
    assert(
      runNumberOfPhases(input, 4, patternMap) === List(0, 1, 0, 2, 9, 4, 9, 8)
    )
  }

  test("Day 16 test part 01 - case 02 - 100 phases") {

    val input = getInput("inputs/day16-02.txt")
    val patternMap: Map[Int, List[Int]] = (1 to input.size).toList.map { x =>
      (x, generatePattern(x, input.size).toList)
    }.toMap
    assert(
      runNumberOfPhases(input, 100, patternMap).take(8) === List(2, 4, 1, 7, 6,
        1, 7, 6)
    )
  }

  test("Day 16 test part 02 - case 03 - 100 phases") {

    val input = getInput("inputs/day16-03.txt")
    val offset = input.take(7).mkString.toInt
    val phaseSize = input.size * 10000 - offset

    assert(
      runNumberOfPhasesPart2(input, 100, phaseSize).take(8) === List(8, 4, 4, 6,
        2, 0, 2, 6)
    )
  }
}
