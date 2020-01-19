class Day05Test extends org.scalatest.FunSuite {
  test("Day05 test 3,0,4,0,99") {
    val test = Vector(3,0,4,0,99)
    assert(Day05.intProg(test, Seq(100), 0)._1 === 100)
  }
  test("Day05 test 1002,4,3,0,4,0,99") {
    val test = Vector(1002,4,3,0,4,0,99)
    assert(Day05.intProg(test, Seq(1), 0)._1 === 12)
  }
  test("Day05 test 3,9,8,9,10,9,4,9,99,-1,8") {
    val test = Vector(3,9,8,9,10,9,4,9,99,-1,8)
    assert(Day05.intProg(test, Seq(8), 0)._1 === 1)
    assert(Day05.intProg(test, Seq(7), 0)._1 === 0)
  }
  test("Day05 test 3,9,7,9,10,9,4,9,99,-1,8") {
    val test = Vector(3,9,7,9,10,9,4,9,99,-1,8)
    assert(Day05.intProg(test, Seq(8), 0)._1 === 0)
    assert(Day05.intProg(test, Seq(7), 0)._1 === 1)
  }
  test("Day05 test 3,3,1108,-1,8,3,4,3,99") {
    val test = Vector(3,3,1108,-1,8,3,4,3,99)
    assert(Day05.intProg(test, Seq(8), 0)._1 === 1)
    assert(Day05.intProg(test, Seq(7), 0)._1 === 0)
  }
  test("Day05 test 3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") {
    val test = Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    assert(Day05.intProg(test, Seq(0), 0)._1 === 0)
    assert(Day05.intProg(test, Seq(2), 0)._1 === 1)
  }
  test("Day05 test 3,3,1105,-1,9,1101,0,0,12,4,12,99,1") {
    val test = Vector(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
    assert(Day05.intProg(test, Seq(0), 0)._1 === 0)
    assert(Day05.intProg(test, Seq(2), 0)._1 === 1)
  }  
}


