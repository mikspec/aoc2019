class Day10Test extends org.scalatest.FunSuite {
  test("Day10 test 01") {
    val data = Seq(
      ".#..#",
      ".....",
      "#####",
      "....#",
      "...##"
    )

    assert(
      Day10.processSkyMap(Day10.createMap(data)) === (Day10.Position(4, 3), 8)
    )
  }
}
