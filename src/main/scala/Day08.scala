import scala.annotation.tailrec

object Day08 extends App {

  val Width = 25
  val Height = 6

  val image = scala.io.Source.fromResource("inputs/day08.txt").getLines().toSeq.flatMap(_.grouped(Width * Height))

  def fewestZerosLayer(layers: Seq[String]) = layers
    .map { x => ((Width * Height - x.replace("1","").size) * (Width * Height - x.replace("2","").size), Width * Height - x.replace("0","").size) }
    .reduce { (x, y) => if (x._2 < y._2) x else y }
    ._1
    
  println(s"Day 08 part1 = ${fewestZerosLayer(image)}")

  @tailrec
  def combineLayers(layerA: String, layerB: String, combined: String): String = {
    if (layerA == "") combined
    else {
      if (layerA.head == '2') combineLayers(layerA.tail, layerB.tail, combined + layerB.head)
      else combineLayers(layerA.tail, layerB.tail, combined + layerA.head)
    }
  }

  def generateImage(layers: Seq[String]) = layers
    .reduce { (x, y) => combineLayers(x, y, "") }
    .grouped(Width)
    .map { _.replace("0"," ") }

  println(s"Day 08 part2") 
  generateImage(image) foreach println
}