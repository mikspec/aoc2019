class Day06Test extends org.scalatest.FunSuite {
  test("Day06 test COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L") {
    val data = Map("COM"->List("B"),"B"->List("C","G"),"C"->List("D"),"D"->List("E","I"),"E"->List("F","J"),"G"->List("H"),"J"->List("K"),"K"->List("L"))
    assert(Day06.distanceMap(data, "COM").foldLeft(0) { (sum, planet) => sum + planet._2 } === 42)
  }
  test("Day06 test COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L,K)YOU,I)SAN") {
    val data = Map("COM"->List("B"),"B"->List("C","G"),"C"->List("D"),"D"->List("E","I"),"E"->List("F","J"),"G"->List("H"),"J"->List("K"),"K"->List("L","YOU"),"I"->List("SAN"))
    val reverseMap = data.flatMap(x => x._2.map(y => y -> x._1))
    assert(Day06.getPathBeeten("YOU", "SAN", reverseMap) === 4)
  }
}


