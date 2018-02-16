import Util.Node._
import Util.Node

import scala.annotation.tailrec

object ReverseLinkedList {

  @tailrec
  def reverse(present: Node, prev: Option[Node] = None): Node = {
    val reversed = present.copy(next = prev)
    present.next match {
      case None => reversed
      case Some(next) => reverse(next, Some(reversed))
    }
  }

  def main(args: Array[String]): Unit = {
    val n1 = Node(1)
    val n2 = Node(2)
    val n3 = Node(3)
    val n4 = Node(4)
    val n5 = Node(5)
    val n6 = Node(6)

    push(n1, n2)
    push(n1, n3)
    push(n1, n4)
    push(n1, n5)
    push(n1, n6)

    println(s"The given list is : ")
    printList(n1)

    val reversed = reverse(n1)
    println(s"The reversed list is : ")
    printList(reversed)
  }

}
