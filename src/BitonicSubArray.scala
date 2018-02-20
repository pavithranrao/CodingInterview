
object BitonicSubArray {

  def main(args: Array[String]): Unit = {

    val array = Array(12, 4, 78, 90, 45, 23)
    val length = array.length

    val increasing = (1 until length).foldLeft(List(1)) {
      (accumulator, idx) =>
        accumulator :+
          (if (array(idx) >= array(idx - 1)) {
            accumulator.last + 1
          } else {
            1
          })
    }

    val decreasing = (length - 2 to 0 by -1).foldLeft(List(1)) {
      (accumulator, idx) =>
        accumulator :+
          (if (array(idx) <= array(idx + 1)) {
            accumulator.last + 1
          } else {
            1
          })
    }

    println(s"Given array : ${array.mkString(", ")}")
    println(s"Increasing array : ${increasing.mkString(", ")}")
    println(s"Decreasing array : ${decreasing.mkString(", ")}")

    val answer = increasing.zip(decreasing).map(x => x._1 + x._2 - 1).max
    println(s"The bitonic array size is : $answer")

  }
}
