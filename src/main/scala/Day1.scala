
object Day1 extends App {


  implicit class Crossable[X](xs: Iterable[X]) {
    def cross[Y](ys: Iterable[Y]) = for {x <- xs; y <- ys} yield (x, y)
  }

  val filename = "day1.txt"

  val lines = (for (line <- io.Source.fromResource(filename).getLines()) yield line.toInt).toList

  val xlines: Iterable[(Int,Int)] = lines cross(lines)

  xlines.foreach { x =>
    if (x._1 + x._2 == 2020)
      println(x._1 * x._2)
  }

  // part 2
  val xxlines = xlines cross lines

  xxlines.foreach { x =>
    if (x._1._1 + x._1._2 + x._2 == 2020)
      println(x._1._1 * x._1._2 * x._2  )

  }



}
