import scala.annotation.tailrec
import scala.util.matching.Regex

object Day14 extends App {

  case class Item(name: String, quantity: Int) 

  val compoundPattern = "(.+)=> (\\d+) ([A-Z]+)".r
  val itemsPattern = "(\\d+) ([A-Z]+)".r

  val recipe = scala.io.Source
    .fromResource("inputs/day14.txt")
    .getLines()
    .map(row => {
      val compoundPattern(items, quantity, name) = row
      Item(name, quantity.toInt) -> {
        itemsPattern
          .findAllMatchIn(items)
          .map(x => {
            Item(x.group(2), x.group(1).toInt)
          })
          .toSeq
      }
    })
    .toMap

  println(
    s"Day 14 part1 ${recipe(Item("FUEL", 1))}"
  )
}
