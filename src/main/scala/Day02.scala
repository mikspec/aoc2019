import scala.annotation.tailrec

object Day02 extends App {

  val progCode = scala.io.Source.fromResource("inputs/day02.txt").getLines.takeWhile(_ != "").next.split(",").map(_.toInt).toVector

  def gravityCalc(prog: Vector[Int]):Int = {

    def regUpdate(reg: Vector[Int], index: Int, oper: (Int,Int) => Int): Vector[Int] = 
      reg.updated(reg(index + 3), oper(reg(reg(index + 1)), reg(reg(index + 2))))

    @tailrec
    def step(reg: Vector[Int], index: Int): Vector[Int] = reg(index) match {
      case 99 => reg
      case 1  => step(regUpdate(reg, index, _ + _), index + 4)
      case 2  => step(regUpdate(reg, index, _ * _), index + 4)
      case _  => throw new IllegalArgumentException("")
    }

    step(prog, 0).head
  }

  println(s"Day 02 part1 = ${gravityCalc(progCode.updated(1, 12).updated(2, 2))}")
  
  def part2(program: Vector[Int]):Int = {
    for (i <- 0 to 99) {
      for (j <- 0 to 99) {
        if (gravityCalc(program.updated(1, i).updated(2, j)) == 19690720) return i*100 + j
      }
    }
    return -1
  }
  
  println(s"Day 02 part2 = ${part2(progCode)}")

  lazy val generateInputs: LazyList[(Int, Int)] =
    LazyList.from(0).flatMap(variations(_).to(LazyList))

  def variations(a: Int): Set[(Int, Int)] =
    (0 to a).flatMap(b => Seq((a, b), (b, a))).toSet

  val part2var2 = generateInputs
      //.map { case (noun, verb) => println(s"Trying: $noun and $verb"); (noun, verb)}
      .find { case (noun, verb) => gravityCalc(progCode.updated(1, noun).updated(2, verb)) == 19690720}
      .map {case (noun, verb) => 100 * noun + verb}
      .get

  println(s"Day 02 part2 variant2 = ${part2var2}")
}