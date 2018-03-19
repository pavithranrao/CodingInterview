import Util.Node

object SortOnAbsVal {

  def sortList(head: Node[Int]): Option[Node[Int]] = {
    var answer: Option[Node[Int]] = None
    var current: Option[Node[Int]] = Some(head.copy())

    while (current.isDefined) {
      val present = current.get.copyNode()
      if (present.value >= 0) {
        if (answer.isDefined) {
          answer.get.push(present)
        } else {
          answer = Some(present)
        }
      } else {
        if (answer.isDefined) {
          present.push(answer.get)
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

    n1.push(n2)
    n1.push(n3)
    n1.push(n4)
    n1.push(n5)

    println("The given sorted on abs value list is : ")
    n1.printList()

    val answer = sortList(n1)
    println("The sorted list is :")
    answer.get.printList()

  }

}
