import Util.dropFirstMatch

object EqualSumSubSets {

  def getEqualSumSubSets(array: Array[Int]): Either[(Array[Int], Array[Int]), _] = {
    import DiscreteKnapSack.Item
    val halfSum = array.sum / 2
    val arrayItems: Array[Item] = array.map { x => (x, x) }

    val (filledCapacity, selectedItems) =
      DiscreteKnapSack.getKnapSack(items = arrayItems, capacity = halfSum)
    val partitionA = selectedItems.unzip._1

    if (filledCapacity == halfSum) {
      val partitionB = partitionA.foldLeft(array.toSeq)(dropFirstMatch).toArray
      //      val partitionB = partitionA.foldLeft(array.toSeq) {
      //        case (acc, element) =>
      //          dropFirstMatch(acc, element)
      //      }.toArray
      Left(partitionA, partitionB)
    } else {
      Right(None)
    }

  }

  def main(args: Array[String]): Unit = {
    val array = Array(1, 2, 6, 7)

    val answer = getEqualSumSubSets(array)
    answer match {
      case Left(partitions) =>
        val (partitionA, partitionB) = partitions
        println(s"The equal sum partitions are : ${partitionA.mkString(", ")}" +
          s" and ${partitionB.mkString(", ")}")
      case Right(_) =>
        println(s"The given array cannot be split into equal sum partitions")
    }

  }

}
