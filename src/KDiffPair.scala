object KDiffPair {

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 4, 7, 1, 2, 5, 1, 2, 3)

    assert(findPairs(nums, 4) == 2)
    assert(findPairs(nums, 0) == 2)
    assert(findPairs(nums, -5) == 0)
  }

  // beats 100%
  // with replacement
  def findPairs(nums: Array[Int], k: Int): Int = {
    if (k < 0) {
      0
    } else {
      if (k > 0) {
        val numsSet = nums.toSet
        numsSet.foldLeft(0) {
          case (count, present) =>
            if (numsSet.contains(present + k)) {
              count + 1
            } else {
              count
            }
        }
      } else {
        //  val numsFreq = nums.foldLeft(Map[Int, Int]().withDefaultValue(0)) {
        //    case (acc, present) =>
        //      acc.updated(present, acc(present) + 1)
        //  }
        val numsFreq = Util.getFreqMap(nums)
        numsFreq.foldLeft(0) {
          case (count, (_, freq)) =>
            if (freq >= 2) {
              count + 1
            } else {
              count
            }
        }
      }
    }
  }

}
