import Day14.{getRecipe, processRecipe, Item}

class Day14Test extends org.scalatest.FunSuite {

  test("Day 14 test part 01 - case 01") {

    val recipe = getRecipe("inputs/day14-01.txt")
    assert(processRecipe(Seq(Item("FUEL", 1, 0)), recipe, Map())("ORE").quantityNeeded === 31)
  }

  test("Day 14 test part 01 - case 02") {

    val recipe = getRecipe("inputs/day14-02.txt")
    assert(processRecipe(Seq(Item("FUEL", 1, 0)), recipe, Map())("ORE").quantityNeeded === 165)
  }

  test("Day 14 test part 01 - case 03") {

    val recipe = getRecipe("inputs/day14-03.txt")
    assert(processRecipe(Seq(Item("FUEL", 1, 0)), recipe, Map())("ORE").quantityNeeded === 13312)
  }
}
