object MajorityElement {

  //  Imperative way
  //  def getMajorityElement[A](nums: Array[A]): A = {
  //    var count = 0
  //    var candidate = nums.head
  //
  //    for (num <- nums) {
  //      if (count == 0) {
  //        candidate = num
  //      }
  //      count += (if (num == candidate) 1 else -1)
  //    }
  //    candidate
  //  }

  // Functional Way
  def getMajorityElement[A](nums: Array[A]): A = {
    nums.foldLeft((nums.head, 0)) {
      case ((candidate, count), num) =>
        (candidate, count) match {
          // if count is zero, make the present number as candidate
          case (_, 0) => (num, 1)
          // if the present number is same as the candidate,     increment count
          case _ if candidate == num => (candidate, count + 1)
          // if the present number is not same as the candidate, decrement count
          case _ => (candidate, count - 1)
        }
    }._1
  }

  def main(args: Array[String]): Unit = {
    val array = Array(7, 7, 5, 7, 5, 1, 5, 7, 5, 5, 7, 7, 5, 5, 5, 5)
    val answer = getMajorityElement(array)
    println(answer)

    assert(getMajorityElement(Array(3, 3, 4)) == 3)
  }

}
