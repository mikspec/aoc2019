import scala.annotation.tailrec

object Day05 extends App {

  val progCode = scala.io.Source.fromResource("inputs/day05.txt").getLines().toSeq.flatMap(_.split(",")).map(_.toInt).toVector
  
  def getDigit(value: Int, pos: Int): Int = (value % (Math.pow(10, pos).toInt)) / Math.pow(10, pos - 1).toInt

  def getParam(reg: Vector[Int], index:Int, mode: Int) = mode match {
    case 0 => reg(reg(index))
    case 1 => reg(index)
  }

  def intProg(prog: Vector[Int], input: Seq[Int], index: Int): (Int, Vector[Int], Int) = {

    def regUpdate(reg: Vector[Int], index: Int, oper: (Int,Int) => Int, param1Mode:Int, param2Mode:Int): Vector[Int] = 
      reg.updated(reg(index + 3), oper(getParam(reg, index + 1, param1Mode), getParam(reg, index + 2, param2Mode)))

    @tailrec
    def step(reg: Vector[Int], index: Int, input: Seq[Int]): (Int, Vector[Int], Int) = {
      reg(index) % 100 match {
        case 99 => (-1, reg, -1)
        case 1  => step(regUpdate(reg, index, _ + _, getDigit(reg(index), 3), getDigit(reg(index), 4)), index + 4, input)
        case 2  => step(regUpdate(reg, index, _ * _, getDigit(reg(index), 3), getDigit(reg(index), 4)), index + 4, input)
        case 3  => step(reg.updated(reg(index + 1), input.head), index + 2, input.tail)
        case 4  => (reg(reg(index + 1)), reg, index + 2)
        case 5  => step(reg, if (getParam(reg, index + 1, getDigit(reg(index), 3)) != 0) getParam(reg, index + 2, getDigit(reg(index), 4)) else index + 3, input)
        case 6  => step(reg, if (getParam(reg, index + 1, getDigit(reg(index), 3)) == 0) getParam(reg, index + 2, getDigit(reg(index), 4)) else index + 3, input)
        case 7  => step(regUpdate(reg, index, (p1, p2) => if (p1 < p2) 1 else 0, getDigit(reg(index), 3), getDigit(reg(index), 4)), index + 4, input)
        case 8  => step(regUpdate(reg, index, (p1, p2) => if (p1 == p2) 1 else 0, getDigit(reg(index), 3), getDigit(reg(index), 4)), index + 4, input)
        case _  => throw new IllegalArgumentException("")
      }
    }
    step(prog, index, input)
  }

  @tailrec
  def executeProg(reg: Vector[Int], conf: Seq[Int], index: Int, output: Int): Int = intProg(reg, conf, index) match {
    case (newOut, newProg, newIndex) => {
      if (newIndex == -1) output
      else executeProg(newProg, conf, newIndex, newOut)
      }
  }
  

  println(s"Day 05 part1 = ${executeProg(progCode, Seq(1), 0, 0)}")
  println(s"Day 05 part2 = ${executeProg(progCode, Seq(5), 0, 0)}")
}