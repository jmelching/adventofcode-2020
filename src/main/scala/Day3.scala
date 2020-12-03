import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Day3 extends App {

  def parseLine(line: String): List[Boolean] = {
    val path = new ListBuffer[Boolean]
    for (c <- line) {
      if (c == '#')
        path.append(true)
      else
        path.append(false)
    }
    path.toList

  }

  val rules = List[(Int, Int)]((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

  val trees = rules.map(rule => {

    val paths = (for (line <- io.Source.fromResource("day3.txt").getLines()) yield parseLine(line)).toList

    val numRows = paths.length
    val numColumns = paths.head.length
    val rightSteps = rule._1
    val downSteps = rule._2

    val treeMatrix = new ArrayBuffer[Array[Boolean]]()

    for (i <- 0 until numRows) {
      val dupes = new ArrayBuffer[Boolean]
      for (j <- 0 until (numRows / numColumns + 1) * rightSteps) {
        dupes.appendAll(paths(i))
      }
      treeMatrix.append(dupes.toArray)
    }

    var columnOffset = rightSteps
    val encounteredPath = new ArrayBuffer[Boolean]

    for (masterRow <- downSteps until numRows by downSteps) {
      encounteredPath.append(treeMatrix(masterRow )(columnOffset))
      columnOffset = columnOffset + rightSteps
    }
    encounteredPath.toList.count(_ == true)

  })

  println(trees)
  println(trees.product)


}
