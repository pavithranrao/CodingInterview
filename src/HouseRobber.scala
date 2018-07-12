object HouseRobber {

  def main(args: Array[String]): Unit = {
    val nums = Array(2, 7, 9, 3, 1)
    assert(rob(nums) == 12)

  }

  // beats 100%
  def rob(nums: Array[Int]): Int = {

    val (even, odd) =
      nums.indices.foldLeft((0, 0)) {
        case ((a, b), idx) =>
          if (idx % 2 == 0) {
            ((a + nums(idx)) max b, b)
          } else {
            (a, a max (b + nums(idx)))
          }
      }

    even max odd
  }

}
