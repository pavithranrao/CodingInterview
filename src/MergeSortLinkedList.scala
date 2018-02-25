import Util.Node

object MergeSortLinkedList {

  def mergeSortedList[A](left: Option[Node[A]], right: Option[Node[A]])
                        (implicit comparatorFn: (A, A) => Boolean): Option[Node[A]] = {

    var head: Option[Node[A]] = None
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

  def mergerSort[A](head: Option[Node[A]])
                   (implicit comparatorFn: (A, A) => Boolean): Option[Node[A]] = {
    if (head.isEmpty || head.get.next.isEmpty) {
      head
    } else {
      val middleElement = head.get.getMiddleElement
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

    n1.push(n4)
    n1.push(n2)
    n1.push(n6)
    n1.push(n3)
    n1.push(n5)


    println("The given list is :")
    n1.printList()
    val comparatorFn = (a: Int, b: Int) => a < b

    println("The sorted list is :")
    val sortedList = mergerSort(Some(n1))(comparatorFn)
    sortedList.get.printList()

  }

}
