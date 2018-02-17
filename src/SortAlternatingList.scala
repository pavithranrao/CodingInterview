import Util.Node
import Util.Node._

// Need to improve this later

object SortAlternatingList {

  def splitList(head: Node): (Option[Node], Option[Node]) = {
    var current: Option[Node] = Some(head)
    var slowPtr: Option[Node] = None
    var fastPtr: Option[Node] = None

    while (current.isDefined) {
      val present = current.get
      val presentNode = copyNode(present)
      if (slowPtr.isEmpty) {
        slowPtr = Some(presentNode)
      } else {
        push(slowPtr.get, presentNode)
      }
      if (present.next.isDefined) {
        val presentNode = copyNode(present.next.get)
        if (fastPtr.isEmpty) {
          fastPtr = Some(presentNode)
        } else {
          push(fastPtr.get, presentNode)
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

    push(n10, n40)
    push(n10, n53)
    push(n10, n30)
    push(n10, n67)
    push(n10, n12)
    push(n10, n89)

    println("The given list is :")
    printList(n10)

    val (left, right) = splitList(n10)
    println(s"The left list is : ")
    printList(left.get)
    println(s"The right list is : ")
    printList(right.get)

    val rightSort = Some(reverse(right.get))
    val comparatorFn = (a: Int, b: Int) => a < b
    val answer = MergeSortLinkedList.mergeSortedList(left, rightSort)(comparatorFn)

    println(s"The sorted list is : ")
    printList(answer.get)
  }

}
