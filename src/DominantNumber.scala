object DominantNumber {

  def main(args: Array[String]): Unit = {
    val nums = Array(3, 6, 1, 0)
    assert(dominantIndex(nums) == 1)
  }

  def dominantIndex(nums: Array[Int]): Int = {
    val m = nums.max
    val check = nums.forall {
      num =>
        num == m || num <= m / 2
    }
    if (check) {
      nums.indexOf(m)
    } else {
      -1
    }
  }

}
