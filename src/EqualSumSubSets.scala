import Util.seqDifference
import DiscreteKnapSack.Item

object EqualSumSubSets {

  def getEqualSumSubSets(array: Array[Int]): Either[(Array[Int], Array[Int]), _] = {
    val halfSum = array.sum.toDouble / 2
    if (halfSum.isValidInt) {
      // Check if the sum is an integer
      // and proceed with 0/1 KnapSack
      val arrayItems: Array[Item] = array.map { x => (x, x) }

      // Call 0/1 KnapSack with both Item's (weight and value) as the number itself
      val (filledCapacity, selectedItems) =
        DiscreteKnapSack.getKnapSack(items = arrayItems, capacity = halfSum.toInt)
      val partitionA = selectedItems.unzip._1

      if (filledCapacity == halfSum) {
        // if the knapsack is completely filled,
        // the array can be partition into equal sum partitions
        val partitionB = seqDifference(array, partitionA).toArray
        Left(partitionA, partitionB)
      } else {
        // if not return `Not possible to partition`
        Right(None)
      }
    } else {
      // if sum is not a perfect integer,
      // return `Not possible to partition`
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

    val negTest = Array(1, 1, 1)
    assert(getEqualSumSubSets(negTest).isRight)

  }

}
