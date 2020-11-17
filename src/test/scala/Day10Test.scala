class Day10Test extends org.scalatest.FunSuite {

  test("Day 10 test part 01") {

    val image1 =
      scala.io.Source.fromResource("inputs/day10-01.txt").getLines().toSeq

    assert(
      Day10.processSkyMap(Day10.createMap(image1)) === (Day10.Position(4, 3), 8)
    )
  }

  test("Day 10 test part 02") {

    val image2 =
      scala.io.Source.fromResource("inputs/day10-02.txt").getLines().toSeq
    val skyMap = Day10.createMap(image2)
    val station = Day10.processSkyMap(skyMap)

    assert(
      Day10.laserRun(
        Day10.laserMap(station._1, (skyMap - station._1).toList),
        null,
        200
      ) === Day10.Position(2, 8)
    )
  }

}
