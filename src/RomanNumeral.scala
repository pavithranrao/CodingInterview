object RomanNumeral {

  val romanNumeral: Map[String, Int] = Map(
    "I" -> 1, "IV" -> 4, "V" -> 5, "IX" -> 9,
    "X" -> 10, "XL" -> 40, "L" -> 50, "XC" -> 90,
    "C" -> 100, "CD" -> 400, "D" -> 500, "CM" -> 900,
    "M" -> 1000)

  val numberRoman: Map[Int, String] = romanNumeral.map {
    case (key, value) =>
      value -> key
  }

  val dividers: Array[Int] = numberRoman.keys.toArray.sortWith(_ > _)

  def romanToNumber(input: String): Int =
    input.foldRight((0, input.last)) {
      case (present, (acc, prevChar)) =>
        if (romanNumeral(present.toString) < romanNumeral(prevChar.toString))
          (acc - romanNumeral(present.toString), present)
        else
          (acc + romanNumeral(present.toString), present)
    }._1


  def numberToRoman(input: Int): String =
    dividers.foldLeft(("", input)) {
      case ((acc, numerator), divider) =>
        (acc + numberRoman(divider) * (numerator / divider), numerator % divider)
    }._1


  def main(args: Array[String]): Unit = {

    val input = "VI"
    val number = romanToNumber(input)
    val roman = numberToRoman(number * 3)

    println(s"The input is : $input")
    println(s"The input in number format is : $number")
    println(s"The input $input * III in roman format is : $roman")
  }

}
