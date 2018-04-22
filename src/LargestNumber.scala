object LargestNumber {

  def getLargestNumber(array: Array[Int]): String = {
    def compare(a: Int, b: Int): Boolean =
      (a.toString + b.toString).toLong > (b.toString + a.toString).toLong

    val answer = array.sortWith(compare).foldLeft(Array[Long]()) {
      (acc, present) =>
        if (present.toLong == 0 && acc.isEmpty) {
          acc
        } else {
          acc :+ present.toLong
        }
    }.mkString("")
    if (answer == "") {
      "0"
    } else {
      answer
    }

    //    array.sortWith(compare).toString
  }

  def main(args: Array[String]): Unit = {
    //    val array = Array(1, 34, 3, 98, 9, 76, 45, 4)
    //    val array = Array(12, 121, 0)
    val array = Array(0, 0)
    val answer = getLargestNumber(array)
    println(s"The largest number formed is $answer")
  }

}
