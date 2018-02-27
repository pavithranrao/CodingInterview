object FractionalKnapSack {
  type Item = (Float, Float)

  def getKnapSack(items: Array[Item], capacity: Float): Array[Item] = {
    val sortedItems = items.map {
      x =>
        val valuePerUnit = x._2 / x._1
        (x._1, x._2, valuePerUnit)
    }.sortWith(_._3 > _._3)

    val finalValue = sortedItems.foldLeft((capacity, Array[Item]())) {
      case ((currentCapacity, acc), (weight, _, valuePerUnit)) =>
        if (currentCapacity != 0) {
          val feasibleWeight = weight min currentCapacity
          (currentCapacity - feasibleWeight,
            acc :+ (feasibleWeight, feasibleWeight * valuePerUnit))
        } else {
          (currentCapacity, acc)
        }
    }._2

    finalValue
  }

  def main(args: Array[String]): Unit = {
    val items = Array((50f, 250f), (100f, 20f), (40f, 80f), (300f, 30f), (50f, 25f))
    val capacity = 150f

    val answer = getKnapSack(items, capacity)
    val (capacityUsed, maxValue) = answer.foldLeft((0f, 0f)) {
      case ((accCap, accVal), (currentCap, currentVal)) =>
        (accCap + currentCap, accVal + currentVal)
    }

    println(s"The maximum value of goods for capacity $capacity is : $maxValue")
    println(s"The capacity used is $capacityUsed out of $capacity")
    println(s"The items are : ${answer.mkString(", ")}")

    assert(capacityUsed == capacity)
  }

}
