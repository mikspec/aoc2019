import scala.annotation.tailrec

object Day03 extends App {

  val pattern = "([URLD]+)(\\d+)".r
  val wires = scala.io.Source.fromResource("inputs/day03.txt")
                .getLines()
                .toSeq.map(
                  _.split(",")
                    .map(inst => {
                        val pattern(command, dist) = inst
                        (command, dist.toInt)  
                      }).toList
                  )

  @tailrec
  def processElement(direction: String, dist: Int, x: Int, y: Int, index: Int, plan: Map[(Int, Int, Int), Int], wireLength: Int): (Map[(Int, Int, Int), Int], Int, Int, Int) = {
    if (dist == 0) (plan, x, y, wireLength)
    else {
      var (xNew, yNew) = (x, y)
      direction match {
        case "R" => xNew += 1
        case "L" => xNew -= 1
        case "U" => yNew -= 1
        case "D" => yNew += 1
      }
      val length = plan.getOrElse((xNew, yNew, index), wireLength + 1)
      processElement(direction, dist - 1, xNew, yNew, index, plan + ((xNew, yNew, index) -> length), wireLength + 1)
    }
  }

  @tailrec
  def processWire(wire: List[(String, Int)], x: Int, y: Int, index: Int, plan: Map[(Int, Int, Int), Int], wireLength: Int): Map[(Int, Int, Int), Int] = {
    val (planNew, xNew, yNew, lengthNew) = processElement(wire.head._1, wire.head._2, x, y, index, plan, wireLength)
    if (wire.tail == Nil) planNew
    else processWire(wire.tail, xNew, yNew, index, planNew, lengthNew)
  }
  
  def processWires(wires: Seq[List[(String, Int)]]): Map[(Int, Int, Int), Int] = {
    wires.zipWithIndex.map(path => processWire(path._1, 0, 0, path._2, Map(), 0)).reduce((path1, path2) => path1 ++ path2)
  }

  def findCross(plan: Map[(Int, Int, Int), Int]) = {
    val cross = plan.groupBy { case ((x, y, _), _) => (x, y) }
      .map { case (k, v) => (k, v.size) }
      .filter { _._2 > 1 }.toList
      .map { _._1 }
      .reduce { (p1, p2) => if (p1._1.abs + p1._2.abs < p2._1.abs + p2._2.abs) p1 else p2 }
    cross._1.abs + cross._2.abs
  }

  def  findCrossForShortestWires(plan: Map[(Int, Int, Int), Int]) = {
    val cross = plan.groupBy { case ((x, y, _), _) => (x, y) }
      .filter { _._2.size > 1 }.toList
      .map { p => (p._1, p._2.toList.foldLeft(0)(_ + _._2)) }
      .reduce { (p1, p2) => if (p1._2 < p2._2) p1 else p2 }
    cross._2
  }

  val plan = processWires(wires)
  println(s"Day 03 part1 = ${findCross(plan)}")
  println(s"Day 03 part2 = ${findCrossForShortestWires(plan)}")
}