object FractionalKnapSack {

  def main(args: Array[String]): Unit = {
    val items = Array[(Int, Int)]((50, 250), (100, 20), (40, 80), (300, 30), (50, 25))

    val capacity = 150

    val sortedItems = items.sortWith {
      case (x, y) =>
        val xRate = x._2.toFloat / x._1.toFloat
        val yRate = y._2.toFloat / y._1.toFloat

        xRate > yRate
    }

    val finalValue = sortedItems.foldLeft((capacity, List[Int]())) {
      case ((currentCapacity, acc), (weight, value)) =>
        //        println(s"$currentCapacity -> $weight $value")
        if (currentCapacity != 0) {
          (currentCapacity - (weight min currentCapacity), acc :+ value)
        } else {
          (currentCapacity, acc)
        }
    }._2

    println(s"The maximum value of goods for capacity $capacity is : ${finalValue.sum}")

  }

}
