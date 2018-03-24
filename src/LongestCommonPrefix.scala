object LongestCommonPrefix {

  def getCommonChars(left: String, right: String): String =
  // takeWhile example
    left.zip(right).takeWhile {
      case (x, y) => x == y
    }.unzip._1.mkString("")

  //    left.zip(right).foldLeft(("", true)) {
  //      case ((acc, isContinuous), (leftChar, rightChar)) =>
  //        if (isContinuous) {
  //          if (leftChar == rightChar) {
  //            (s"$acc${leftChar.toString}", isContinuous)
  //          } else {
  //            (acc, false)
  //          }
  //        } else {
  //          (acc, isContinuous)
  //        }
  //    }._1

  // List is chosen because it has `::` that splits
  // the given list to head and tail
  def getLongestPrefix(input: List[String]): String =
    input match {
      case (head :: Nil) => head
      case (left :: right :: Nil) =>
        getCommonChars(left, right)
      case head :: tail =>
        getCommonChars(head, getLongestPrefix(tail))
      case _ => ""
    }


  def main(args: Array[String]): Unit = {
    val list = List("geeksforgeeks", "geeks", "geek", "geezer")
    // val list = List("apple", "ape", "april")
    println(s"The given list of strings is : ${list.mkString(", ")}")

    val answer = getLongestPrefix(list)
    if (answer != "")
      println(s"The common prefix in all the given string is : $answer")
    else
      println("There is no prefix in the given list of strings")
  }

}
