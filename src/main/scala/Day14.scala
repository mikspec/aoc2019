import scala.annotation.tailrec
import scala.util.matching.Regex

object Day14 extends App {

  case class Item(name: String, quantity: Int)
  case class Recipe(quantity: Int, components: Seq[Item])

  def getRecipe(file: String): Map[String, Recipe] = {

    val compoundPattern = "(.+) => (\\d+) ([A-Z]+)".r
    val itemsPattern = "(\\d+) ([A-Z]+)".r
    
    scala.io.Source
      .fromResource(file)
      .getLines()
      .map(row => {
        val compoundPattern(items, quantity, name) = row
        name -> {
          Recipe(
            quantity.toInt,
            itemsPattern
              .findAllMatchIn(items)
              .map(x => {
                Item(x.group(2), x.group(1).toInt)
              })
              .toSeq
          )
        }
      })
      .toMap
  }

  val recipe = getRecipe("inputs/day14.txt")

  println(
    s"Day 14 part1 ${recipe}"
  )
}
