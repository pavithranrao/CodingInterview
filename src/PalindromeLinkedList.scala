import Util.Node

object PalindromeLinkedList {
  def main(args: Array[String]): Unit = {
    val n1 = Node(1)
    val n2 = Node(2)
    val n3 = Node(3)
    val n4 = Node(4)
    val n5 = Node(2)
    val n6 = Node(1)

    n1.push(n2)
    n1.push(n3)
    n1.push(n4)
    n1.push(n5)
    n1.push(n6)

    n1.printList()
    val answer = checkPalindrome(n1)
    println(s"The answer is : $answer")

  }


  def checkPalindrome[A](node: Node[A]): Boolean = {
    var head = node

    def checkNode(presentNode: Node[A]): Boolean = {
      if (presentNode.value == head.value) {
        if (head.next.isDefined) {
          head = head.next.get
        }
        true
      } else {
        false
      }
    }

    def _rev(node: Node[A], check: Boolean): Boolean = {
      if (node.next.isDefined) {
        val returnCheck = _rev(node.next.get, check)
        returnCheck && checkNode(node)
      } else {
        checkNode(node)
      }
    }

    _rev(node, check = true)
  }
}
