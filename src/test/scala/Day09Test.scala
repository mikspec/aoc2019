class Day09Test extends org.scalatest.FunSuite {
  test("Day09 test 104,1125899906842624,99") {
    val test: Map[BigInt, BigInt] = Map(
      BigInt(0) -> BigInt(104),
      BigInt(1) -> BigInt(1125899906842624L),
      BigInt(2) -> BigInt(99)
    )
    assert(
      Day09.executeProg(test, Seq(), 0, Seq(), 0) === Seq(1125899906842624L)
    )
  }

  test("Day09 test 109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99") {
    val test: Map[BigInt, BigInt] = Map(
      BigInt(0) -> BigInt(109),
      BigInt(1) -> BigInt(1),
      BigInt(2) -> BigInt(204),
      BigInt(3) -> BigInt(-1),
      BigInt(4) -> BigInt(1001),
      BigInt(5) -> BigInt(100),
      BigInt(6) -> BigInt(1),
      BigInt(7) -> BigInt(100),
      BigInt(8) -> BigInt(1008),
      BigInt(9) -> BigInt(100),
      BigInt(10) -> BigInt(16),
      BigInt(11) -> BigInt(101),
      BigInt(12) -> BigInt(1006),
      BigInt(13) -> BigInt(101),
      BigInt(14) -> BigInt(0),
      BigInt(15) -> BigInt(99)
    )
    assert(Day09.executeProg(test, Seq(), 0, Seq(), 0) === Seq(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))
  }
}
