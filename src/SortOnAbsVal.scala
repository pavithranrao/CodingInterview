import Util.Node
import Util.Node._

object SortOnAbsVal {

  def sortList(head: Node): Option[Node] = {
    var answer: Option[Node] = None
    var current: Option[Node] = Some(head.copy())

    while (current.isDefined) {
      val present = copyNode(current.get)
      if (present.value >= 0) {
        if (answer.isDefined) {
          push(answer.get, present)
          // implies => push(answer.get, present, isSingleNode = true)
        } else {
          answer = Some(present)
        }
      } else {
        if (answer.isDefined) {
          push(present, answer.get, isSingleNode = false)
          answer = Some(present.copy())
        } else {
          answer = Some(present)
        }
      }
      current = current.get.next
    }
    answer

  }

  def main(args: Array[String]): Unit = {
    // 1 -> -2 -> -3 -> 4 -> -5
    val n1 = Node(1)
    val n2 = Node(-2)
    val n3 = Node(-3)
    val n4 = Node(4)
    val n5 = Node(-5)

    push(n1, n2)
    push(n1, n3)
    push(n1, n4)
    push(n1, n5)
    println("The given sorted on abs value list is : ")
    printList(n1)

    val answer = sortList(n1)
    println("The sorted list is :")
    printList(answer.get)

  }

}
