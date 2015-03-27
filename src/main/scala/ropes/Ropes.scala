package ropes

case class Rope(from: Int, to: Int)

object Ropes extends App {

  import scala.io._
  val lines = Source.fromFile("src/main/scala/ropes/A-large-practice.in").getLines
  lines.next // drop #cases
  var c = 1

  for (line <- lines) {
    val count = line.toInt

    val ropes = (0 until count).map(_ => lines.next).map(line => Rope(line.split(" ")(0).toInt, line.split(" ")(1).toInt))
    val is = intersections(ropes)

    println(s"Case #$c: $is")
    c = c+1
  }
  // val ropes = Rope(1,1) :: Rope(2,2) :: Nil
  // val ropes = Rope(1,10) :: Rope(5,5) :: Rope(7,7) :: Nil
  //  println(intersections(ropes))

  def intersections(ropes: Seq[Rope]): Int =
    ropes.map(rope => ropes.filter(other => intersect(other, rope)).size).sum / 2

  def intersect(r1: Rope, r2: Rope): Boolean =
    ((r1.from > r2.from) && (r1.to < r2.to)) ||
      ((r1.from < r2.from) && (r1.to > r2.to))
}

