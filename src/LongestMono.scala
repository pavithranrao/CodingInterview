import scala.collection.mutable.ArrayBuffer

object LongestMono {

  def getLongestSeq(array: Array[Int]): Int = {

    val length = array.length
    val incArray = Array.fill(length)(0)

    for (i <- length - 1 to 1 by -1;
         j <- length - 1 until i by -1) {
      if (array(i) < array(j)) {
        if (incArray(j) == 0) {
          incArray(j) = 1
        }
        incArray(i) = incArray(i) max (incArray(j) + 1)
      }
    }

    var max = Int.MinValue
    var min = Int.MaxValue

    var start = 1
    var end = 1

    for (i <- 1 until length) {
      if (incArray(i) > max) {
        max = incArray(i)
        start = i
      }
      if (incArray(i) <= min) {
        min = incArray(i)
        end = i
      }
    }

    // println(s"Start : $start, End : $end")
    val output = ArrayBuffer[Int]()
    var marker = incArray(start) + 1
    for (i <- start to end) {
      if (incArray(i) < marker) {
        // println(array(i))
        output += array(i)
        marker = incArray(i)
      }
    }

    // output.toArray
    output.length
  }

  def main(args: Array[String]): Unit = {
    // val array = Array(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)
    val array = Array(10, 9, 2, 5, 3, 7, 101, 18)
    println(s"The given array is : ${array.mkString(", ")}")

    val answer = getLongestSeq(array)

    println(s"The length of the longest increasing sub sequence" +
      s" is : $answer")
  }

}
