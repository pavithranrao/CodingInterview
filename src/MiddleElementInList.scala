import Util.Node

object MiddleElementInList {

  def main(args: Array[String]): Unit = {
    val n1 = Node(1)
    val n2 = Node(2)
    val n3 = Node(3)
    val n4 = Node(4)
    val n5 = Node(5)
    val n6 = Node(6)

    n1.next = Some(n2)
    n2.next = Some(n3)
    n3.next = Some(n4)
    n4.next = Some(n5)
    n5.next = Some(n6)

    var slowPtr = n1
    var fastPtr = n1.next.get

    while (fastPtr.next.isDefined && fastPtr.next.get.next.isDefined) {
      fastPtr = fastPtr.next.get.next.get
      slowPtr = slowPtr.next.get
    }

    println(s"The middle element is : ${slowPtr.value}")

  }

}
