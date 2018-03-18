object EqualSumSubSets {

  // Courtesy : https://alvinalexander.com/scala/how-to-drop-filter-remove-first-matching-element-in-sequence-list
  def dropFirstMatch[A](ls: Seq[A], value: A): Seq[A] = {

    val index = ls.indexOf(value)
    index.compare(0) match {
      // index is -1 if there is no match
      case -1 => ls
      // if the element is at the beginning of the Seq
      case 0 => ls.tail
      // if the element is elsewhere
      case _ =>
        val (a, b) = ls.splitAt(index)
        a ++ b.tail
    }
  }

  def getEqualSumSubSets(array: Array[Int]): Either[(Array[Int], Array[Int]), _] = {
    import DiscreteKnapSack.Item
    val halfSum = array.sum / 2
    val arrayItems: Array[Item] = array.map { x => (x, x) }

    val (filledCapacity, selectedItems) =
      DiscreteKnapSack.getKnapSack(items = arrayItems, capacity = halfSum)
    val partitionA = selectedItems.unzip._1

    if (filledCapacity == halfSum) {
      val partitionB = partitionA.foldLeft(array.toSeq) {
        case (acc, element) =>
          dropFirstMatch(acc, element)
      }.toArray
      Left(partitionA, partitionB)
    } else {
      Right(false)
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
