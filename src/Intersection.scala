import scala.reflect.ClassTag

object Intersection {

  def main(args: Array[String]): Unit = {
    val nums1 = Array(1, 2, 2, 1)
    val nums2 = Array(2, 2)
    val answer = intersection(nums1, nums2)
    println(answer.mkString(", "))
  }

  // FP
  // Map extra space
  def intersection[A: ClassTag](nums1: Array[A], nums2: Array[A]): Array[A] = {

    val freqMap = Util.getFreqMap(nums1)
    nums2.foldLeft(freqMap, Array[A]()) {
      case ((freq, intersection), present) =>
        if (freq.contains(present) && freq(present) > 0) {
          (freq.updated(present, freq(present) - 1),
            intersection :+ present)
        } else {
          (freq, intersection)
        }
    }._2.distinct
  }

  // faster implementation
  def intersection2(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val toSet = nums1.toSet
    nums2.filter(toSet.contains).distinct
  }

}
