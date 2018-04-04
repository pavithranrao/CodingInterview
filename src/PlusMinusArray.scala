object PlusMinusArray {

  def plusMinus(arr: Array[Int]): Unit = {
    val length = arr.length
    val count = arr.foldLeft((0f, 0f, 0f)) {
      case ((pos, zero, neg), present) =>
        present.compare(0) match {
          case 1 => (pos + 1, zero, neg)
          case -1 => (pos, zero, neg + 1)
          case 0 => (pos, zero + 1, neg)
        }
    }
    println(s"${count._1 / length}\n${count._3 / length}\n${count._2 / length}")
  }

  def main(args: Array[String]) {
    val arr = Array(-4, 3, -9, 0, 4, 1)
    plusMinus(arr)

  }

}
