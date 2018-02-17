import Util.Node
import Util.Node._

object MergeSortLinkedList {

  def mergeSortedList(left: Option[Node], right: Option[Node])
                     (implicit comparatorFn: (Int, Int) => Boolean): Option[Node] = {

    var head: Option[Node] = None
    if (left.isEmpty) {
      right
    } else if (right.isEmpty) {
      left
    } else {
      if (comparatorFn(left.get.value, right.get.value)) {
        head = left
        head.get.next = mergeSortedList(left.get.next, right)
      } else {
        head = right
        head.get.next = mergeSortedList(left, right.get.next)
      }
      head
    }
  }

  def mergerSort(head: Option[Node])
                (implicit comparatorFn: (Int, Int) => Boolean): Option[Node] = {
    if (head.isEmpty || head.get.next.isEmpty) {
      head
    } else {
      val middleElement = getMiddleElement(head)
      // println(s"The middle element is ${middleElement.get.value}")
      val nextOfMiddle = middleElement.get.next
      middleElement.get.next = None

      val left = mergerSort(head)
      val right = mergerSort(nextOfMiddle)

      mergeSortedList(left, right)
    }
  }

  def main(args: Array[String]): Unit = {
    val n1 = Node(1)
    val n2 = Node(2)
    val n3 = Node(3)
    val n4 = Node(4)
    val n5 = Node(5)
    val n6 = Node(6)

    push(n1, n4)
    push(n1, n2)
    push(n1, n6)
    push(n1, n3)
    push(n1, n5)

    println("The given list is :")
    printList(n1)


    val comparatorFn = (a: Int, b: Int) => a < b

    println("\nThe sorted list is :")
    val sortedList = mergerSort(Some(n1))(comparatorFn)
    printList(sortedList.get)

  }

}
