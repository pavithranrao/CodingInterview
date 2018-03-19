import Util.Node

// Need to improve this later

object SortAlternatingList {

  def splitList(head: Node[Int]): (Option[Node[Int]], Option[Node[Int]]) = {
    var current: Option[Node[Int]] = Some(head)
    var slowPtr: Option[Node[Int]] = None
    var fastPtr: Option[Node[Int]] = None

    while (current.isDefined) {
      val present = current.get
      val presentNode = present.copyNode()
      if (slowPtr.isEmpty) {
        slowPtr = Some(presentNode)
      } else {
        slowPtr.get.push(presentNode)
      }
      if (present.next.isDefined) {
        val presentNode = present.next.get.copyNode()
        if (fastPtr.isEmpty) {
          fastPtr = Some(presentNode)
        } else {
          fastPtr.get.push(presentNode)
        }
      }
      if (present.next.isEmpty) {
        current = None
      } else if (present.next.get.next.isDefined) {
        current = present.next.get.next
      } else {
        current = present.next
      }
      // println(s"Slow* is : $slowPtr")
      // println(s"Fast* is : $fastPtr")
    }

    (slowPtr, fastPtr)
  }

  def main(args: Array[String]): Unit = {
    val n10 = Node(10)
    val n40 = Node(40)
    val n53 = Node(53)
    val n30 = Node(30)
    val n67 = Node(67)
    val n12 = Node(12)
    val n89 = Node(89)

    n10.push(n40)
    n10.push(n53)
    n10.push(n30)
    n10.push(n67)
    n10.push(n12)
    n10.push(n89)

    println("The given list is :")
    n10.printList()

    val (left, right) = splitList(n10)
    println(s"The left list is : ")
    left.get.printList()
    println(s"The right list is : ")
    right.get.printList()

    val rightSort = Some(right.get.reverse())
    val comparatorFn = (a: Int, b: Int) => a < b
    val answer = MergeSortLinkedList.mergeSortedList(left, rightSort)(comparatorFn)

    println(s"The sorted list is : ")
    answer.get.printList()
  }

}
