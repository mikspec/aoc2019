import scala.annotation.tailrec

object Day09 extends App {

  val progCode = scala.io.Source
    .fromResource("inputs/day09.txt")
    .getLines()
    .toSeq
    .flatMap(_.split(","))
    .map { x => BigInt(x.toLong) }
    .zipWithIndex
    .map { case (value, index) => BigInt(index) -> value }
    .toMap

  def getDigit(value: BigInt, pos: Int): Int =
    (value.toInt % (Math.pow(10, pos).toInt)) / Math.pow(10, pos - 1).toInt

  def getParam(
      reg: Map[BigInt, BigInt],
      index: BigInt,
      mode: Int,
      relativeBaseOffset: BigInt
  ): BigInt =
    mode match {
      case 0 => reg.getOrElse(reg.getOrElse(index, 0), 0)
      case 1 => reg.getOrElse(index, 0)
      case 2 => reg.getOrElse(relativeBaseOffset + reg.getOrElse(index, 0), 0)
      case _ => throw new IllegalArgumentException("")
    }

  case class State(
      output: BigInt,
      stack: Map[BigInt, BigInt],
      index: BigInt,
      relativeBaseOffset: BigInt
  )

  def intProg(
      prog: Map[BigInt, BigInt],
      input: Seq[BigInt],
      index: BigInt,
      relativeBaseOffset: BigInt
  ): State = {

    def regUpdate(
        reg: Map[BigInt, BigInt],
        index: BigInt,
        oper: (BigInt, BigInt) => BigInt,
        param1Mode: Int,
        param2Mode: Int,
        param3Mode: Int,
        relativeBaseOffset: BigInt
    ): Map[BigInt, BigInt] =
      reg.updated(
        if (param3Mode == 0) reg.getOrElse(index + 3, 0)
        else relativeBaseOffset + reg.getOrElse(index + 3, 0),
        oper(
          getParam(reg, index + 1, param1Mode, relativeBaseOffset),
          getParam(reg, index + 2, param2Mode, relativeBaseOffset)
        )
      )

    @tailrec
    def step(
        reg: Map[BigInt, BigInt],
        index: BigInt,
        input: Seq[BigInt],
        relativeBaseOffset: BigInt
    ): State = {
      (reg.getOrElse(index, BigInt(0)) % 100).toInt match {
        case 99 => State(-1, reg, -1, -1)
        case 1 =>
          step(
            regUpdate(
              reg,
              index,
              _ + _,
              getDigit(reg.getOrElse(index, 0), 3),
              getDigit(reg.getOrElse(index, 0), 4),
              getDigit(reg.getOrElse(index, 0), 5),
              relativeBaseOffset
            ),
            index + 4,
            input,
            relativeBaseOffset
          )
        case 2 =>
          step(
            regUpdate(
              reg,
              index,
              _ * _,
              getDigit(reg.getOrElse(index, 0), 3),
              getDigit(reg.getOrElse(index, 0), 4),
              getDigit(reg.getOrElse(index, 0), 5),
              relativeBaseOffset
            ),
            index + 4,
            input,
            relativeBaseOffset
          )
        case 3 =>
          step(
            reg.updated(
              if (getDigit(reg.getOrElse(index, 0), 3) == 0)
                reg.getOrElse(index + 1, 0)
              else relativeBaseOffset + reg.getOrElse(index + 1, 0),
              input.head
            ),
            index + 2,
            input.tail,
            relativeBaseOffset
          )
        case 4 =>
          State(
            getParam(
              reg,
              index + 1,
              getDigit(reg.getOrElse(index, 0), 3),
              relativeBaseOffset
            ),
            reg,
            index + 2,
            relativeBaseOffset
          )
        case 5 =>
          step(
            reg,
            if (
              getParam(
                reg,
                index + 1,
                getDigit(reg.getOrElse(index, 0), 3),
                relativeBaseOffset
              ) != 0
            )
              getParam(
                reg,
                index + 2,
                getDigit(reg.getOrElse(index, 0), 4),
                relativeBaseOffset
              )
            else index + 3,
            input,
            relativeBaseOffset
          )
        case 6 =>
          step(
            reg,
            if (
              getParam(
                reg,
                index + 1,
                getDigit(reg.getOrElse(index, 0), 3),
                relativeBaseOffset
              ) == 0
            )
              getParam(
                reg,
                index + 2,
                getDigit(reg.getOrElse(index, 0), 4),
                relativeBaseOffset
              )
            else index + 3,
            input,
            relativeBaseOffset
          )
        case 7 =>
          step(
            regUpdate(
              reg,
              index,
              (p1, p2) => if (p1 < p2) 1 else 0,
              getDigit(reg.getOrElse(index, 0), 3),
              getDigit(reg.getOrElse(index, 0), 4),
              getDigit(reg.getOrElse(index, 0), 5),
              relativeBaseOffset
            ),
            index + 4,
            input,
            relativeBaseOffset
          )
        case 8 =>
          step(
            regUpdate(
              reg,
              index,
              (p1, p2) => if (p1 == p2) 1 else 0,
              getDigit(reg.getOrElse(index, 0), 3),
              getDigit(reg.getOrElse(index, 0), 4),
              getDigit(reg.getOrElse(index, 0), 5),
              relativeBaseOffset
            ),
            index + 4,
            input,
            relativeBaseOffset
          )
        case 9 =>
          step(
            reg,
            index + 2,
            input,
            relativeBaseOffset + getParam(
              reg,
              index + 1,
              getDigit(reg.getOrElse(index, 0), 3),
              relativeBaseOffset
            )
          )
        case _ => throw new IllegalArgumentException("")
      }
    }
    step(prog, index, input, relativeBaseOffset)
  }

  @tailrec
  def executeProg(
      reg: Map[BigInt, BigInt],
      conf: Seq[BigInt],
      index: BigInt,
      output: Seq[BigInt],
      relativeBaseOffset: BigInt
  ): Seq[BigInt] = intProg(reg, conf, index, relativeBaseOffset) match {
    case State(newOut, newProg, newIndex, newRelativeBaseOffset) => {
      if (newIndex == -1) output
      else
        executeProg(
          newProg,
          conf,
          newIndex,
          output :+ newOut,
          newRelativeBaseOffset
        )
    }
  }

  println(s"Day 09 part1 = ${executeProg(progCode, Seq(1), 0, Seq(), 0)}")
  println(s"Day 09 part2 = ${executeProg(progCode, Seq(2), 0, Seq(), 0)}")
}
