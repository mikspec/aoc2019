import scala.annotation.tailrec

object Day07 extends App {

  val progCode = scala.io.Source.fromResource("inputs/day07.txt").getLines().toSeq.flatMap(_.split(",")).map(_.toInt)
  
  def permute(xs: List[Int]): List[List[Int]] = {
    if(xs.size < 2) {
      List(xs)
    } else {
      for {
        x <- xs
        y <- permute(xs diff List(x))
      } yield x :: y
    }
  }
  
  @tailrec
  def executeChain(confList: List[Int], input: Int, prog: Seq[Int]): Int = {
    if (confList.isEmpty) input
    else executeChain(confList.tail, Day05.intProg(prog.toVector, Seq(confList.head, input), 0)._1, prog)
  }

  def maxSignal(config: List[Int], prog: Seq[Int]): Int = 
    permute(config)
      .map { conf => executeChain(conf, 0, prog) }
      .max

  case class Prog (stack: Vector[Int], index: Int, input: Seq[Int], output: Int)

  def executeLoop(config: List[Int], program: Seq[Int]) = {

    val modules = List(
      Prog(program.toVector, 0, Seq(config(0), 0), 0), 
      Prog(program.toVector, 0, Seq(config(1)), 0), 
      Prog(program.toVector, 0, Seq(config(2)), 0), 
      Prog(program.toVector, 0, Seq(config(3)), 0), 
      Prog(program.toVector, 0, Seq(config(4)), 0), 
    )

    @tailrec
    def executeOne(modules: List[Prog]): Int = {
      if (modules.tail.isEmpty) modules.head.output
      else {      
        Day05.intProg(modules.head.stack, modules.head.input, modules.head.index) match {
          case (newOut, newProg, newIndex) => { 
            if (newIndex == -1) executeOne(
              Prog(modules.tail.head.stack, modules.tail.head.index, modules.tail.head.input :+ modules.head.output, modules.tail.head.output) +: modules.tail.tail
            )
            else executeOne(
              Prog(modules.tail.head.stack, modules.tail.head.index, modules.tail.head.input :+ newOut, modules.tail.head.output) 
              +: modules.tail.tail
              :+ Prog(newProg, newIndex, Seq(), newOut)
            )
          }
        }
      }
    }

    executeOne(modules)
  }

  def maxSignalLoop(config: List[Int], prog: Seq[Int]): Int = 
    permute(config)
      .map { conf => executeLoop(conf, prog) }
      .max

  println(s"Day 07 part1 = ${maxSignal(List(0,1,2,3,4), progCode)}")
  println(s"Day 07 part2 = ${maxSignalLoop(List(5,6,7,8,9), progCode)}")
}