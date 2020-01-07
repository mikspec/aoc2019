class Day02Test extends org.scalatest.FunSuite {
  test("Day02 test 1,0,0,0,99") {
    val test1 = Array(1,0,0,0,99)
    assert(Day02.gravityCalc(test1) === 2)
  }
  test("Day02 test 1,1,1,4,99,5,6,0,99") {
    val test2 = Array(1,1,1,4,99,5,6,0,99)
    assert(Day02.gravityCalc(test2) === 30)
  }
}

