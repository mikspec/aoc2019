import scala.annotation.tailrec
object Day16 extends App {

  val PATTERN: List[Int] = List(0, 1, 0, -1)

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
      sourcePattern: List[Int]
  ): LazyList[Int] = {

    val pattern = multiplyPattern(n, sourcePattern, n, List()).toVector

    def loop(n: Int, position: Int): LazyList[Int] =
      pattern(position % pattern.size) #:: loop(n, position + 1)
    loop(n, 0)
  }

  println(s"Day 16 part 1 ${generatePattern(3, PATTERN).drop(1).take(650).toList}")
}
