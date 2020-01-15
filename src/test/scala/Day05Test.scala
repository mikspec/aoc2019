class Day05Test extends org.scalatest.FunSuite {
  test("Day05 test 3,0,4,0,99") {
    val test = Vector(3,0,4,0,99)
    assert(Day05.intProg(test, 100) === 100)
  }
  test("Day05 test 1002,4,3,0,4,0,99") {
    val test = Vector(1002,4,3,0,4,0,99)
    assert(Day05.intProg(test, 1) === 12)
  }
  test("Day05 test 3,9,8,9,10,9,4,9,99,-1,8") {
    val test = Vector(3,9,8,9,10,9,4,9,99,-1,8)
    assert(Day05.intProg(test, 8) === 1)
    assert(Day05.intProg(test, 7) === 0)
  }
  test("Day05 test 3,9,7,9,10,9,4,9,99,-1,8") {
    val test = Vector(3,9,7,9,10,9,4,9,99,-1,8)
    assert(Day05.intProg(test, 8) === 0)
    assert(Day05.intProg(test, 7) === 1)
  }
  test("Day05 test 3,3,1108,-1,8,3,4,3,99") {
    val test = Vector(3,3,1108,-1,8,3,4,3,99)
    assert(Day05.intProg(test, 8) === 1)
    assert(Day05.intProg(test, 7) === 0)
  }
  test("Day05 test 3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") {
    val test = Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    assert(Day05.intProg(test, 0) === 0)
    assert(Day05.intProg(test, 2) === 1)
  }
  test("Day05 test 3,3,1105,-1,9,1101,0,0,12,4,12,99,1") {
    val test = Vector(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
    assert(Day05.intProg(test, 0) === 0)
    assert(Day05.intProg(test, 2) === 1)
  }  
}


