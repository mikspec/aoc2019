object Day02 extends App {

  val progArray = scala.io.Source.fromResource("inputs/day02.txt").getLines.takeWhile(_ != "").next.split(",").map(_.toInt)

  def gravityCalc(prog: Array[Int]):Int = {
    for (i <- 0 until prog.length by 4) {
       if (prog(i) == 99) return prog(0)
       if (prog(i + 3) >= prog.length) prog.padTo(prog(i + 3), -1) 
       if ((prog(i + 1) >= prog.length) || (prog(i + 1) < 0)) return -1
       if ((prog(i + 2) >= prog.length) || (prog(i + 2) < 0)) return -1
       if (prog(i) == 1) prog(prog(i + 3)) = prog(prog(i + 1)) + prog(prog(i + 2))
       else if (prog(i) == 2) prog(prog(i + 3)) = prog(prog(i + 1)) * prog(prog(i + 2))
       else return -1
    }
    return prog(0)
  }

  progArray(1) = 12
  progArray(2) = 2
  println(s"Day 02 part1 = ${gravityCalc(progArray.clone())}")
  
  def part2(program: Array[Int]):Int = {
    for (i <- 0 to 99) {
      for (j <- 0 to 99) {
        program(1) = i
        program(2) = j
        if (gravityCalc(program.clone()) == 19690720) return i*100 + j
      }
    }
    return -1
  }
  
  println(s"Day 02 part2 = ${part2(progArray.clone())}")
}