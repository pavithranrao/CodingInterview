import scala.annotation.tailrec

object AssignCookies {

  // greedy algorithm
  // I need to improve thinking
  def main(args: Array[String]): Unit = {
    val g = Array(1, 2)
    val s = Array(1, 2, 3)

    assert(findContentChildren(g, s) == 2)
  }

  // beats 100%
  def findContentChildren(g: Array[Int], s: Array[Int]): Int = {

    // By sorting the child greed and cookie size,
    // we can find the smallest cookie that can satisfy the smallest greedy child
    val child = g.sorted
    val size = s.sorted

    val numChild = g.length
    val numCookies = s.length

    @tailrec
    def feedChild(childIdx: Int = 0,
                  sizeIdx: Int = 0): Int = {
      if ((childIdx == numChild) || (sizeIdx == numCookies)) {
        // reached end of either / both of the array
        // return the number of children fed
        childIdx
      } else {
        // check if the cookie size can feed the child
        // if yes, mark childIdx as assigned and move to next child
        // else try with bigger cookie
        if (size(sizeIdx) >= child(childIdx)) {
          feedChild(childIdx + 1, sizeIdx + 1)
        } else {
          feedChild(childIdx, sizeIdx + 1)
        }
      }
    }

    feedChild()
  }

}
