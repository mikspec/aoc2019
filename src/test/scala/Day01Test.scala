class Day01Test extends org.scalatest.FunSuite {
  test("Day01 test 12") {
    assert(Day01.fuelCalc(12,0) === 2)
  }
  test("Day01 test 100756") {
    assert(Day01.fuelCalc(100756,0) === 50346)
  }
}
