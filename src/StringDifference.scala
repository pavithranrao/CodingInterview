object StringDifference {

  def findTheDifference(s: String, t: String): Char = {
    s.zip(t).foldLeft(t.last.toInt) {
      case (acc, (a, b)) =>
        acc ^ a ^ b
    }.toChar
  }


  def main(args: Array[String]): Unit = {
    val s = "a"
    val t = "aa"

    val answer = findTheDifference(s, t)
    println(s"$answer")
  }

}
