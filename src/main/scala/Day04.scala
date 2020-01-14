object Day04 extends App {

  val START = 265275
  val END   = 781584 

  def generateNext(end: Int)(curr: Int) = {
    setDigits(curr, end.toString.length, false, true, 0, end)
  }  

  def setDigits(curr: Int, position: Int, hasDouble: Boolean, incPos: Boolean, accum: Int, endVal: Int): Int = {
    if (position == 0) return accum
    
    var incPosNew = incPos
    var head = getDigit(if (incPos) curr else accum, position)
    val prev = getDigit(accum, position + 1)
    
    if (head < prev) {
      head = prev
      incPosNew = false
    }
    else if (position == 1) {
      if (!hasDouble) {
        if (head > prev) return -1
        else {
          if (head == prev && incPosNew) return -1
          head = prev
          incPosNew = false
        }
      }
      if (incPosNew) head += 1
      if (head == 10) return -1
    }

    var hasDoubleNew = hasDouble || (head == prev)
    var accumNew = setDigits(curr, position - 1, hasDoubleNew, incPosNew, accum + head * Math.pow(10, position - 1).toInt, endVal)
    
    if (accumNew == -1) {
      head += 1
      incPosNew = false
      hasDoubleNew = hasDouble || (head == prev)
      if (head == 10) accumNew = -1
      else accumNew = setDigits(curr, position - 1, hasDoubleNew, incPosNew, accum + head * Math.pow(10, position - 1).toInt, endVal)
    } 
    if (accumNew > endVal) return -1
    accumNew 
  }

  def getDigit(value: Int, pos: Int): Int = (value % (Math.pow(10, pos).toInt)) / Math.pow(10, pos - 1).toInt

  def part1(start: Int, end:Int) = LazyList.iterate(start - 1)(generateNext(end)).drop(1).takeWhile(_ > -1).length

  def part2(start: Int, end:Int) = LazyList.iterate(start - 1)(generateNext(end)).drop(1).takeWhile(_ > -1).count(checkDouble)

  def checkDouble(number: Int):Boolean = {
    number
      .toString
      .toSeq
      .foldLeft(Map[Char, Int]()) {(stats: Map[Char, Int], curr: Char) => stats + (curr -> (stats.getOrElse(curr, 0) + 1)) }
      .toList
      .filter((_._2 == 2)).length > 0
  }

  println(s"Day 04 part1 = ${part1(START, END)}")
  println(s"Day 04 part2 = ${part2(START, END)}")
}