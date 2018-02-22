import scala.annotation.tailrec

object Util {

  //  case class Node(nodeValue: Int) {
  //    val value: Int = nodeValue
  //    var next: Option[Node] = None
  //  }

  case class Node(value: Int, var next: Option[Node] = None)

  object Node {
    def push(head: Node, element: Node, isSingleNode: Boolean = true): Unit = {
      var current = head
      while (current.next.isDefined) {
        current = current.next.get
      }

      current.next = Some(element)
      if (isSingleNode) {
        element.next = None
      }
    }

    def printList(head: Node): Unit = {
      var current = Option(head)
      while (current.isDefined) {
        print(s" -> ${current.get.value}")
        current = current.get.next
      }
      println("\n")
    }


    def getMiddleElement(head: Option[Node]): Option[Node] = {
      var slowPtr = head.get
      var fastPtr = head.get.next.get

      while (fastPtr.next.isDefined && fastPtr.next.get.next.isDefined) {
        fastPtr = fastPtr.next.get.next.get
        slowPtr = slowPtr.next.get
      }

      Some(slowPtr)
    }

    def sort(head: Option[Node])
            (implicit comparatorFn: (Int, Int) => Boolean): Option[Node] = {
      MergeSortLinkedList.mergerSort(head)
    }

    def copyNode(node: Node): Node = {
      node.copy(next = None)
    }

    def length(head: Node): Int = {
      var length = 0
      var current: Option[Node] = Some(head)
      while (current.isDefined) {
        length += 1
        current = current.get.next
      }

      length
    }

    // Courtesy : https://gist.github.com/pathikrit/4af1c1c2c590c2478b0f
    @tailrec
    def reverse(present: Node, prev: Option[Node] = None): Node = {
      // Read : https://alvinalexander.com/scala/scala-case-class-copy-method-example
      val reversed = present.copy(next = prev)
      present.next match {
        case None => reversed
        case Some(next) => reverse(next, Some(reversed))
      }
    }
  }

  def swap[A](array: Array[A], source: Int, dest: Int): Unit = {
    val temp = array(source)
    array(source) = array(dest)
    array(dest) = temp
  }

  def invertMap[A, B](inputMap: Map[A, B]): Map[B, List[A]] = {
    inputMap.foldLeft(Map[B, List[A]]()) {
      case (mapAccumulator, (value, key)) =>
        if (mapAccumulator.contains(key)) {
          mapAccumulator.updated(key, mapAccumulator(key) :+ value)
        } else {
          mapAccumulator.updated(key, List(value))
        }
    }
  }
}
