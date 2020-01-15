import scala.annotation.tailrec

object Day05 extends App {

  val progCode = scala.io.Source.fromResource("inputs/day05.txt").getLines().toSeq.flatMap(_.split(",")).map(_.toInt).toVector
  
  def getDigit(value: Int, pos: Int): Int = (value % (Math.pow(10, pos).toInt)) / Math.pow(10, pos - 1).toInt

  def getParam(reg: Vector[Int], index:Int, mode: Int) = mode match {
    case 0 => reg(reg(index))
    case 1 => reg(index)
  }

  def intProg(prog: Vector[Int], input: Int):Int = {

    def regUpdate(reg: Vector[Int], index: Int, oper: (Int,Int) => Int, param1Mode:Int, param2Mode:Int): Vector[Int] = 
      reg.updated(reg(index + 3), oper(getParam(reg, index + 1, param1Mode), getParam(reg, index + 2, param2Mode)))

    @tailrec
    def step(reg: Vector[Int], index: Int, input: Int, output: Int): Int = {
      reg(index) % 100 match {
        case 99 => output
        case 1  => step(regUpdate(reg, index, _ + _, getDigit(reg(index), 3), getDigit(reg(index), 4)), index + 4, input, 0)
        case 2  => step(regUpdate(reg, index, _ * _, getDigit(reg(index), 3), getDigit(reg(index), 4)), index + 4, input, 0)
        case 3  => step(reg.updated(reg(index + 1), input), index + 2, input, 0)
        case 4  => step(reg, index + 2, input, reg(reg(index + 1)))
        case 5  => step(reg, if (getParam(reg, index + 1, getDigit(reg(index), 3)) != 0) getParam(reg, index + 2, getDigit(reg(index), 4)) else index + 3, input, 0)
        case 6  => step(reg, if (getParam(reg, index + 1, getDigit(reg(index), 3)) == 0) getParam(reg, index + 2, getDigit(reg(index), 4)) else index + 3, input, 0)
        case 7  => step(regUpdate(reg, index, (p1, p2) => if (p1 < p2) 1 else 0, getDigit(reg(index), 3), getDigit(reg(index), 4)), index + 4, input, 0)
        case 8  => step(regUpdate(reg, index, (p1, p2) => if (p1 == p2) 1 else 0, getDigit(reg(index), 3), getDigit(reg(index), 4)), index + 4, input, 0)
        case _  => throw new IllegalArgumentException("")
      }
    }

    step(prog, 0, input, 0)
  }

  println(s"Day 05 part1 = ${intProg(progCode, 1)}")
  println(s"Day 05 part2 = ${intProg(progCode, 5)}")
}