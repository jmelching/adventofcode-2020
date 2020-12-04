import scala.collection.mutable.ListBuffer
import scala.reflect.ClassManifest
import scala.util.{Failure, Success, Try}


object Day4 extends App {

  def createCaseClass[T](vals: Map[String, Object])(implicit cmf: ClassManifest[T]) = {
    val ctor = cmf.erasure.getConstructors().head

    val result = Try {
      val args = cmf.erasure.getDeclaredFields().map(f => {
        if (vals.contains(f.getName)) {

          if ("cid".equals(f.getName))
            Some(vals(f.getName))
          else
            vals(f.getName)
        } else {
          if ("cid".equals(f.getName))
            Some("")
        }
      })
      ctor.newInstance(args: _*).asInstanceOf[T]
    }

    result match {
      case Success(value) =>
        Some(value)
      case Failure(value) =>
        None
    }

  }

  case class Passport(ecl: String, pid: String, eyr: String, hcl: String, byr: String, iyr: String, hgt: String, cid: Option[String] = Some("")) {

    def isValid(): Boolean = {
      //      byr (Birth Year) - four digits; at least 1920 and at most 2002.

      val validByr = () => {
        val birthYear = byr.toInt
        birthYear >= 1920 && birthYear <= 2002
      }

      //      iyr (Issue Year) - four digits; at least 2010 and at most 2020.
      val validIyr = () => {
        val issueYear = iyr.toInt
        issueYear >= 2010 && issueYear <= 2020
      }

      //      eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
      val validEyr = () => {
        val expirationYear = eyr.toInt
        expirationYear >= 2020 && expirationYear <= 2030
      }

      // hgt (Height) - a number followed by either cm or in:
      //      If cm, the number must be at least 150 and at most 193.
      //      If in, the number must be at least 59 and at most 76.
      val validHgt = () => {
        val units = hgt.substring(hgt.length - 2)
        val measure = hgt.substring(0, hgt.length - 2)

        if ("cm".equals(units)) {
          val m = measure.toInt
          m >= 150 && m <= 193
        } else if ("in".equals(units)) {
          val m = measure.toInt
          m >= 59 && m <= 76
        } else {
          false
        }
      }

      //      hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
      val validHcl = () => {
        hcl.matches("#([a-f0-9]{6})")
      }

      //        ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
      val validEcl = () => {
        val res = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(this.ecl)
        res
      }
      //      pid (Passport ID) - a nine-digit number, including leading zeroes.
      val validPid = () => {
        pid.matches("[0-9]{9}")
      }

      List(validByr, validIyr, validEyr, validHgt, validHcl, validEcl, validPid).view.map(s => s()).force.forall(_ == true)

    }
  }

  def parseLine(line: String): Array[Option[(String, String)]] = {
    val attributes = line.split(' ')
    val pairs = attributes.map { x =>
      x.split(':') match {
        case y if y.length == 2 => Some((y(0), y(1)))
        case _ => None
      }
    }
    pairs
  }

  val pairs = (for (line <- io.Source.fromResource("day4.txt").getLines()) yield parseLine(line)).flatten.toList
  var currentPassport = Map[String, String]().empty
  val passports = new ListBuffer[Option[Passport]]

  for (pair <- pairs) {
    if (pair.isDefined) {
      pair.map(x => currentPassport += (x._1 -> x._2))
    } else {
      val p = createCaseClass[Passport](currentPassport)
      passports.append(p)
      currentPassport = Map[String, String]().empty
    }
  }

  // don't forget the last one
  val p = createCaseClass[Passport](currentPassport)
  passports.append(p)

  println(passports.count(_.isDefined))
  // part 2
  println(passports.count(x => x.isDefined && x.get.isValid()))


}
