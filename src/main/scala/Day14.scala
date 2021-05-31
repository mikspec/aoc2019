import scala.annotation.tailrec
import scala.util.matching.Regex

object Day14 extends App {

  case class Item(name: String, quantityNeeded: Long, quantityProduced: Long)
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
            Item(name, quantity.toLong, 0),
            itemsPattern
              .findAllMatchIn(items)
              .map(x => {
                Item(x.group(2), x.group(1).toLong, 0)
              })
              .toSeq
          )
        }
      })
      .toMap
  }

  @tailrec
  def multiplyItems(
      factor: Long,
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

  val recipe = getRecipe("inputs/day14.txt")

  @tailrec
  def processRecipe(
      productList: Seq[Item],
      fullRecipe: Map[String, Recipe],
      accum: Map[String, Item]
  ): Map[String, Item] = {

    if (productList.size == 0) accum
    else {
      val product = productList.head
      val output = accum.getOrElse(
        product.name,
        Item(product.name, 0, 0)
      )
      var newAccum = accum
      var components: Seq[Item] = Seq()

      if (
        product.quantityNeeded <= (output.quantityProduced - output.quantityNeeded)
      )
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
        val quantity =
          product.quantityNeeded - (output.quantityProduced - output.quantityNeeded)
        val recipe = fullRecipe.getOrElse(product.name, Recipe(product, Seq()))
        val factor =
          (quantity / recipe.product.quantityNeeded) + (if (
                                                          quantity % recipe.product.quantityNeeded > 0
                                                        ) 1
                                                        else 0)
        components = multiplyItems(factor, recipe.components, Seq())
        newAccum = accum.updated(
          product.name,
          Item(
            product.name,
            output.quantityNeeded + product.quantityNeeded,
            output.quantityProduced + recipe.product.quantityNeeded * factor
          )
        )
      }

      processRecipe(components ++ productList.tail, fullRecipe, newAccum)
    }
  }

  val oreNeeded = processRecipe(Seq(Item("FUEL", 1, 0)), recipe, Map())("ORE").quantityNeeded

  println(
    s"Day 14 part 1 ${oreNeeded}"
  )

  @tailrec
  def processPart2(
      recipe: Map[String, Recipe],
      low: Long,
      high: Long
  ): Long = {
    if (low + 1 >= high) low
    else {
      val state = processRecipe(Seq(Item("FUEL", (high+low)/2, 0)), recipe, Map())
      if (state("ORE").quantityNeeded > 1000000000000L) processPart2(recipe, low, (high+low)/2)  
      else processPart2(recipe, (high+low)/2 ,high)
    }
  }

  println(s"Day 14 part 2 ${processPart2(recipe, 1000000000000L/oreNeeded, 100000000000L)}")
}
