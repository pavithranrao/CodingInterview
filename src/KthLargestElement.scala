import scala.collection.mutable

object KthLargestElement {
  def main(args: Array[String]): Unit = {
    val nums = Array(3, 2, 1, 5, 6, 4)
    val k = 2
    val answer = findKthLargest(nums, k)

    assert(answer == 5)

    val answer2 = findKthLargest(Array(3, 2, 3, 1, 2, 4, 5, 5, 6), 4)
    println(answer2)

  }

  def findKthLargest(nums: Array[Int], k: Int): Int = {
    val maxHeap = mutable.PriorityQueue[Int]()
    maxHeap ++= nums
    for (_ <- 0 until k - 1)
      maxHeap.dequeue()

    maxHeap.dequeue()

  }

}
