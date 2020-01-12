class Day03Test extends org.scalatest.FunSuite {
  test("Day03 test processElement R1") {
    val plan = Day03.processElement("R", 1, 0, 0, 0, Map(), 0)
    assert(plan === (Map((1, 0, 0) -> 1), 1, 0, 1))
  }

  test("Day03 test processElement R2") {
    val plan = Day03.processElement("R", 2, 0, 0, 0, Map(), 0)
    assert(plan === (Map((1, 0, 0) -> 1, (2, 0, 0) -> 2), 2, 0, 2))
  }

  test("Day03 test processElement U2") {
    val plan = Day03.processElement("U", 2, 0, 0, 0, Map(), 0)
    assert(plan === (Map((0, -1, 0) -> 1, (0, -2, 0) -> 2), 0, -2, 2))
  }

  test("Day03 test processElement D2") {
    val plan = Day03.processElement("D", 2, 0, 0, 0, Map(), 0)
    assert(plan === (Map((0, 1, 0) -> 1, (0, 2, 0) -> 2), 0, 2, 2))
  }

  test("Day03 test processWires seq one wire") {
    val plan = Day03.processWires(Seq(List(("R", 2), ("U", 1))))
    assert(plan === Map((1, 0, 0) -> 1, (2, 0, 0) -> 2, (2, -1, 0) -> 3))
  }

  test("Day03 test processWires seq one wire with duplication") {
    val plan = Day03.processWires(Seq(List(("R", 2), ("L", 1), ("U", 1))))
    assert(plan === Map((1, 0, 0) -> 1, (2, 0, 0) -> 2, (1, -1, 0) -> 4))
  }

  test("Day03 test processWires seq two wires") {
    val plan = Day03.processWires(Seq(List(("R", 2), ("U", 1)), List(("D", 2))))
    assert(plan === Map((1, 0, 0) -> 1, (2, 0, 0) -> 2, (2, -1, 0) -> 3, (0, 1, 1) -> 1, (0, 2, 1) -> 2))
  }

  test("Day03 test processWires wires cross") {
    val plan = Day03.processWires(Seq(List(("R", 1), ("U", 1)), List(("U", 1), ("R", 1))))
    assert(plan === Map((1, 0, 0) -> 1, (1, -1, 0) -> 2, (0, -1, 1) -> 1, (1, -1, 1) -> 2))
  }

  test("Day03 test findCross") {
    val dist = Day03.findCross(Day03.processWires(Seq(List(("R", 1), ("U", 1)), List(("U", 1), ("R", 1)))))
    assert(dist === 2)
  }

  test("Day03 test findCrossForShortestWires") {
    val plan = Day03.processWires(Seq(List(("R", 1), ("U", 1)), List(("U", 1), ("R", 1))))
    val dist = Day03.findCrossForShortestWires(plan)
    assert(dist === 4)
  }

  test("Day03 test R75,D30,R83,U83,L12,D49,R71,U7,L72 and U62,R66,U55,R34,D71,R55,D58,R83") {
    val pattern = "([URLD]+)(\\d+)".r
    val wires = Seq("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")
                  .map(
                    _.split(",")
                      .map(inst => {
                        val pattern(command, dist) = inst
                        (command, dist.toInt)  
                        }).toList
                    )    
    val plan = Day03.processWires(wires)
    val dist1 = Day03.findCross(plan)
    val dist2 = Day03.findCrossForShortestWires(plan)
    assert(dist1 === 159)
    assert(dist2 === 610)
  }

  test("Day03 test R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51 and U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") {
    val pattern = "([URLD]+)(\\d+)".r
    val wires = Seq("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
                  .map(
                    _.split(",")
                      .map(inst => {
                        val pattern(command, dist) = inst
                        (command, dist.toInt)  
                        }).toList
                    )    
    val plan = Day03.processWires(wires)
    val dist2 = Day03.findCrossForShortestWires(plan)
    assert(dist2 === 410)
  }
}

