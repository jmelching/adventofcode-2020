object Day2 extends App {

  case class Rule(min: Int, max: Int, letter: Char)

  def extractValidity(): List[(Rule, String)] = {

    def parseLine(line:String): (Rule,String) = {
      val pieces = line.split(':')
      val rulePieces = pieces(0).split(' ')
      (Rule(rulePieces(0).split('-')(0).toInt, rulePieces(0).split('-')(1).toInt, rulePieces(1).head), pieces(1).trim)
    }

    val rules = (for (line <- io.Source.fromResource("day2.txt").getLines()) yield parseLine(line)).toList
    rules
  }

  def isValidRange(ruleTextPair: (Rule, String)): Int = {
    val occurrences = ruleTextPair._2.count(_ == ruleTextPair._1.letter)
    val result = occurrences  match {
      case x if ruleTextPair._1.min to ruleTextPair._1.max contains x => 1
      case _ => 0
    }
    result
  }

  def isValidPositionally(ruleTextPair: (Rule, String)): Int = {
    val firstPositionValid =ruleTextPair._2.charAt(ruleTextPair._1.min - 1) == ruleTextPair._1.letter
    val secondPositionValid =ruleTextPair._2.charAt(ruleTextPair._1.max - 1) == ruleTextPair._1.letter
    val result = if (firstPositionValid && secondPositionValid)
      0
    else if (firstPositionValid || secondPositionValid)
      1
    else
      0

    result
  }

  val letters = 'a' to 'z'
  val positions = 1 to 26

  val letterMappings = letters zip positions

  val rules = extractValidity()

  val validRuleRangeCount = rules.map(isValidRange).sum
  println(s"Total Rules: ${rules.length}, Valid Rules ${validRuleRangeCount}")

  val validRulePositionCount = rules.map(isValidPositionally).sum
  println(s"Total Rules: ${rules.length}, Valid Rules ${validRulePositionCount}")






}
