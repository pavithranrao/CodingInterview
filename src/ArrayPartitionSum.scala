object ArrayPartitionSum {
  def arrayPairSum(nums: Array[Int]): Int = {
    nums.sorted.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).sum
  }

  def main(args: Array[String]): Unit = {
    val array = (1 to 10).toArray
    val answer = arrayPairSum(array)
    println(s"The smallest sum possible is $answer")

  }

}
