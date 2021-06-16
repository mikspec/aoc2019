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
      patternMap: Map[Int, List[Int]],
      counter: Int,
      output: List[Int]
  ): List[Int] = {

    if (counter == input.size) output
    else {
      val sum = input
        .zip { patternMap(counter + 1) }
        .map { x => x._1 * x._2 }
        .sum
        .abs

      runPhase(input, patternMap, counter + 1, output :+ sum % 10)
    }
  }

  @tailrec
  def runNumberOfPhases(input: List[Int], num: Int, patternMap: Map[Int, List[Int]]): List[Int] = {
    if (num == 0) input
    else {
      val newInput = runPhase(input, patternMap, 0, List())
      runNumberOfPhases(newInput, num - 1, patternMap)
    }
  }

  val input: List[Int] = getInput("inputs/day16.txt")
  val patternMap: Map[Int, List[Int]] = (1 to input.size).toList.map { x => (x, generatePattern(x, input.size).toList) }.toMap

  println(
     s"\nDay 16 part 1 ${runNumberOfPhases(input, 100, patternMap).take(8).mkString}"
   )

  val offset = input.take(7).mkString.toInt
  val phaseSize = input.size * 10000 - offset

  @tailrec
  def runPhasePart2(
      phaseSize: Int,
      input: Vector[Int],
      counter: Int,
      output: List[Int],
      sum: Int
  ): List[Int] = {
    if (counter == phaseSize) output
    else {
      val newSum = sum + input(input.size - (counter % input.size) - 1)
      runPhasePart2(
        phaseSize,
        input,
        counter + 1,
        (newSum % 10) +: output,
        newSum
      )
    }
  }

  @tailrec
  def runNumberOfPhasesPart2(
      input: List[Int],
      num: Int,
      phaseSize: Int
  ): List[Int] = {
    if (num == 0) input
    else {
      val newInput = runPhasePart2(phaseSize, input.toVector, 0, List(), 0)
      runNumberOfPhasesPart2(newInput, num - 1, phaseSize)
    }
  }

  println(
    s"Day 16 part 2 ${runNumberOfPhasesPart2(input, 100, phaseSize).take(8).mkString}"
  )

}
