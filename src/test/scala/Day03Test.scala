class Day03Test extends org.scalatest.FunSuite {
  test("Day03 test processElement R1") {
    val plan = Day03.processElement("R", 1, 0, 0, 0, Map())
    assert(plan === (Map((1, 0, 0) -> 1), 1, 0))
  }

  test("Day03 test processElement R2") {
    val plan = Day03.processElement("R", 2, 0, 0, 0, Map())
    assert(plan === (Map((1, 0, 0) -> 1, (2, 0, 0) -> 1), 2, 0))
  }

  test("Day03 test processElement U2") {
    val plan = Day03.processElement("U", 2, 0, 0, 0, Map())
    assert(plan === (Map((0, -1, 0) -> 1, (0, -2, 0) -> 1), 0, -2))
  }

  test("Day03 test processElement D2") {
    val plan = Day03.processElement("D", 2, 0, 0, 0, Map())
    assert(plan === (Map((0, 1, 0) -> 1, (0, 2, 0) -> 1), 0, 2))
  }

  test("Day03 test processWires seq one wire") {
    val plan = Day03.processWires(Seq(List(("R", 2), ("U", 1))))
    assert(plan === Map((1, 0, 0) -> 1, (2, 0, 0) -> 1, (2, -1, 0) -> 1))
  }

  test("Day03 test processWires seq two wires") {
    val plan = Day03.processWires(Seq(List(("R", 2), ("U", 1)), List(("D", 2))))
    assert(plan === Map((1, 0, 0) -> 1, (2, 0, 0) -> 1, (2, -1, 0) -> 1, (0, 1, 1) -> 1, (0, 2, 1) -> 1))
  }

  test("Day03 test processWires wires cross") {
    val plan = Day03.processWires(Seq(List(("R", 1), ("U", 1)), List(("U", 1), ("R", 1))))
    assert(plan === Map((1, 0, 0) -> 1, (1, -1, 0) -> 1, (0, -1, 1) -> 1, (1, -1, 1) -> 1))
  }

  test("Day03 test findCross") {
    val dist = Day03.findCross(Seq(List(("R", 1), ("U", 1)), List(("U", 1), ("R", 1))))
    assert(dist === 2)
  }

  test("Day03 test findCross R75,D30,R83,U83,L12,D49,R71,U7,L72 and U62,R66,U55,R34,D71,R55,D58,R83") {
    val pattern = "([URLD]+)(\\d+)".r
    val wires = Seq("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")
                  .map(
                    _.split(",")
                      .map(inst => {
                        val pattern(command, dist) = inst
                        (command, dist.toInt)  
                        }).toList
                    )    
    val dist = Day03.findCross(wires)
    assert(dist === 159)
  }
}

