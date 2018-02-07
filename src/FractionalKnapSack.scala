object FractionalKnapSack {

  def main(args: Array[String]): Unit = {
    val items = Array[(Int, Int)]((50, 250), (100, 20), (40, 80), (300, 30), (50, 25))

    val capacity = 150

    val sortedItems = items.map {
      x =>
        val rate = x._2.toFloat / x._1.toFloat
        (x._1, x._2, rate)
    }.sortWith(_._3 > _._3)

    val finalValue = sortedItems.foldLeft((capacity, List[Int]())) {
      case ((currentCapacity, acc), (weight, value, _)) =>
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
