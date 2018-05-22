object MissingNumber {

  def main(args: Array[String]): Unit = {
    val nums = Array(9, 6, 4, 2, 3, 5, 7, 0, 1)
    val answer = missingNumber(nums)

    println(s"The missing number is $answer")
    assert(missingNumber2(nums) == answer)
  }

  // xor of bits
  def missingNumber(nums: Array[Int]): Int =
    nums.indices.foldLeft(nums.length) {
      case (acc, idx) =>
        acc ^ idx ^ nums(idx)
    }


  // slower
  // two iterations
  def missingNumber2(nums: Array[Int]): Int =
    nums.length * (nums.length + 1) / 2 - nums.sum

}
