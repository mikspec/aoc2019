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
  def processElement(direction: String, dist: Int, x: Int, y: Int, index: Int, plan: Map[(Int, Int, Int), Int]): (Map[(Int, Int, Int), Int], Int, Int) = {
    if (dist == 0) (plan, x, y)
    else {
      var (xNew, yNew) = (x, y)
      direction match {
        case "R" => xNew += 1
        case "L" => xNew -= 1
        case "U" => yNew -= 1
        case "D" => yNew += 1
      }
      processElement(direction, dist - 1, xNew, yNew, index, plan + ((xNew, yNew, index) -> (plan.getOrElse((xNew, yNew, index), 0) + 1)))
    }
  }

  @tailrec
  def processWire(wire: List[(String, Int)], x: Int, y: Int, index: Int, plan: Map[(Int, Int, Int), Int]): Map[(Int, Int, Int), Int] = {
    val (planNew, xNew, yNew) = processElement(wire.head._1, wire.head._2, x, y, index, plan)
    if (wire.tail == Nil) planNew
    else processWire(wire.tail, xNew, yNew, index, planNew)
  }
  
  def processWires(wires: Seq[List[(String, Int)]]): Map[(Int, Int, Int), Int] = {
    wires.zipWithIndex.map(path => processWire(path._1, 0, 0, path._2, Map())).reduce((path1, path2) => path1 ++ path2)
  }

  def findCross(wires: Seq[List[(String, Int)]]) = {
    val cross = processWires(wires)
      .groupBy { case ((x, y, _), _) => (x, y) }
      .map { case (k, v) => (k, v.size) }
      .filter { _._2 > 1 }.toList
      .map { _._1 }
      .reduce { (p1, p2) => if (p1._1.abs + p1._2.abs < p2._1.abs + p2._2.abs) p1 else p2 }
    cross._1.abs + cross._2.abs
  }

  println(s"Day 03 part1 = ${findCross(wires)}")
}