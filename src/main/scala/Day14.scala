import scala.annotation.tailrec
import scala.util.matching.Regex

object Day14 extends App {

  case class Item(name: String, quantityNeeded: Int, quantityProduced: Int)
  case class Recipe(product: Item, components: Seq[Item])

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
            Item(name, quantity.toInt, 0),
            itemsPattern
              .findAllMatchIn(items)
              .map(x => {
                Item(x.group(2), x.group(1).toInt, 0)
              })
              .toSeq
          )
        }
      })
      .toMap
  }

  @tailrec
  def multiplyItems(
      factor: Int,
      items: Seq[Item],
      accum: Seq[Item]
  ): Seq[Item] = {
    if (items.size == 0) accum
    else
      multiplyItems(
        factor,
        items.tail,
        accum :+ Item(
          items.head.name,
          factor * items.head.quantityNeeded,
          items.head.quantityProduced
        )
      )
  }

  val recipe = getRecipe("inputs/day14-01.txt")

  def processRecipe(
      productList: Seq[Item],
      fullRecipe: Map[String, Recipe],
      accum: Map[String, Item]
  ): Map[String, Item] = {

    if (productList.size == 0) return accum

    val product = productList.head
    val output = accum.getOrElse(
      product.name,
      Item(product.name, 0, 0)
    )

    var newAccum = accum

    if (product.quantityNeeded <= (output.quantityProduced - output.quantityProduced))
      // Enough produced ingredients
      newAccum = accum.updated(
        product.name,
        Item(
          product.name,
          product.quantityNeeded + output.quantityNeeded,
          output.quantityProduced
        )
      )
    else {
      val quantity = product.quantityNeeded - output.quantityProduced
      val recipe = fullRecipe.getOrElse(product.name, Recipe(product, Seq()))
      val factor =
        (quantity / recipe.product.quantityNeeded) + (if (
                                                        quantity % recipe.product.quantityNeeded > 0
                                                      ) 1
                                                      else 0)
      val components = multiplyItems(factor, recipe.components, Seq())
      newAccum = accum.updated(
        product.name,
        Item(
          product.name,
          output.quantityNeeded + quantity,
          output.quantityProduced + recipe.product.quantityNeeded * factor
        )
      )
      newAccum = processRecipe(components, fullRecipe, newAccum)
    }

    processRecipe(productList.tail, fullRecipe, newAccum)
  }

  println(s"Day 14 part1 ${processRecipe(Seq(Item("FUEL", 1, 0)), recipe, Map())}")
}
