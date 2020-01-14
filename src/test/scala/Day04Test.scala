class Day04Test extends org.scalatest.FunSuite {
  test("Day04 test getDigit") {
    assert(Day04.getDigit(123456, 6) === 1)
    assert(Day04.getDigit(123456, 5) === 2)
    assert(Day04.getDigit(123456, 1) === 6)
    assert(Day04.getDigit(123456, 7) === 0)
  }

  test("Day04 test setDigit"){
    assert(Day04.setDigits(9, 2, false, true, 0, 100) === 11)
    assert(Day04.setDigits(10, 2, false, true, 0, 100) === 11)
    assert(Day04.setDigits(11, 2, false, true, 0, 100) === 22)
    assert(Day04.setDigits(12, 2, false, true, 0, 100) === 22)
    assert(Day04.setDigits(22, 2, false, true, 0, 100) === 33)
    assert(Day04.setDigits(31, 2, false, true, 0, 100) === 33)
    assert(Day04.setDigits(89, 2, false, true, 0, 100) === 99)
    assert(Day04.setDigits(99, 2, false, true, 0, 100) === -1)
    assert(Day04.setDigits(120, 3, false, true, 0, 200) === 122)
    assert(Day04.setDigits(110, 3, false, true, 0, 200) === 111)
    assert(Day04.setDigits(112, 3, false, true, 0, 200) === 113)
    assert(Day04.setDigits(299, 3, false, true, 0, 400) === 333)
    assert(Day04.setDigits(333, 3, false, true, 0, 400) === 334)
    assert(Day04.part1(10, 99) === 9)
  }

  test("Day04 test checkDouble") {
    assert(Day04.checkDouble(77888) === true)
    assert(Day04.checkDouble(777888) === false)
  }
}

