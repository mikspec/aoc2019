import scala.annotation.tailrec

object Day16 extends App {

  def getSourcePattern(): List[Int] = List(0, 1, 0, -1)

  def getInput(file: String): List[Int] = {
    scala.io.Source
      .fromResource(file)
      .toList
      .map(digit => digit - '0'.toInt)
  }

  @tailrec
  def multiplyPattern(
      m: Int,
      sourcePattern: List[Int],
      accumCounter: Int,
      accumList: List[Int]
  ): List[Int] = {
    if (sourcePattern.isEmpty) accumList
    else {
      if (accumCounter > 0)
        multiplyPattern(
          m,
          sourcePattern,
          accumCounter - 1,
          accumList :+ sourcePattern.head
        )
      else multiplyPattern(m, sourcePattern.tail, m, accumList)
    }
  }

  def generatePattern(
      n: Int, 
      size: Int
  ): LazyList[Int] = {

    val pattern = multiplyPattern(n, getSourcePattern(), n, List()).toVector

    def loop(n: Int, position: Int): LazyList[Int] =
      pattern(position % pattern.size) #:: loop(n, position + 1)
    loop(n, 0).drop(1).take(size)
  }

  @tailrec
  def runPhase(
      input: List[Int],
      counter: Int,
      output: List[Int]
  ): List[Int] = {

    if (counter == input.size) output
    else {
      val sum = input
        .zip(generatePattern(counter + 1, input.size))
        .map { x => x._1 * x._2 }
        .sum
        .abs

      runPhase(input, counter + 1, output :+ sum % 10)
    }
  }

  @tailrec
  def runNumberOfPhases(input: List[Int], num: Int): List[Int] = {
    if (num == 0) input
    else {
      val newImput = runPhase(input, 0, List())
      println(s"Phase ${num} ${newImput.take(8)}")
      runNumberOfPhases(newImput, num - 1)
    }
  }

  println(
    s"Day 16 part 1 ${runNumberOfPhases(getInput("inputs/day16.txt"), 100).take(8)}"
  )
}
