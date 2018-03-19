import Util.Node

import scala.annotation.tailrec
import scala.reflect.ClassTag

object ReverseLinkedList {

  @tailrec
  def reverse[A : ClassTag](present: Node[A], prev: Option[Node[A]] = None): Node[A] = {
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

    n1.push(n2)
    n1.push(n3)
    n1.push(n4)
    n1.push(n5)
    n1.push(n6)

    println(s"The given list is : ")
    n1.printList()

    val reversed = reverse(n1)
    println(s"The reversed list is : ")
    reversed.printList()
  }

}
