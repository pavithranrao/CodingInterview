object KadanesAlgorithm {


  def getMaxSum(array: Array[Int]): Int = {
    val head = array.head
    val tail = array.tail

    tail.foldLeft(head, head) {
      case ((maxUpToHere, maxSoFar), present) =>
        val maxEndingHere = present max (maxUpToHere + present)
        (maxEndingHere, maxEndingHere max maxSoFar)
    }._2
  }

  def getMaxProduct(array: Array[Int]): Int = {
    array.foldLeft((1, 1, 0)) {
      case ((maxUpToHere, minUpToHere, maxSoFar), present) =>
        present.compare(0) match {
          case 0 =>
            (1, 1, maxSoFar)

          case 1 =>
            val maxEndingHere = maxUpToHere * present
            val minEndingHere = minUpToHere * present min 1
            (maxEndingHere, minEndingHere, maxEndingHere max maxSoFar)

          case -1 =>
            val maxEndingHere = minUpToHere * present max 1
            val minEndingHere = maxUpToHere * present
            (maxEndingHere, minEndingHere, maxSoFar max maxEndingHere max minEndingHere)
        }
    }._3
  }

  def main(args: Array[String]): Unit = {

    val array = Array(9, 6, -3, 7, -2, -4, 0, 1, 10, 5, 7, -10)
    // val array = Array(-10, 9)
    println(s"Given array : ${array.mkString(", ")}")

    val maxSum = getMaxSum(array)
    println(s"Largest Sum is : $maxSum")

    val maxProduct = getMaxProduct(array)
    println(s"Largest Product is : $maxProduct")
  }

}
