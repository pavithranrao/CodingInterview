import scala.annotation.tailrec

object SearchBST {

  // beats 100%
  @tailrec
  def searchBST(root: TreeNode)
               (implicit target: Int): TreeNode = {
    if (root == null) {
      null
    } else {
      root.value.compare(target) match {
        case 0 => root
        case 1 => searchBST(root.left)
        case -1 => searchBST(root.right)
      }
    }
  }

  @tailrec
  def searchBST2(root: TreeNode)(
    implicit target: Int): TreeNode = {
    Option(root) match {
      case None => null
      case _ if target == root.value => root
      case _ if target < root.value => searchBST2(root.left)
      case _ if target > root.value => searchBST2(root.right)
    }
  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(2)
    val left = new TreeNode(1)
    val right = new TreeNode(3)

    root.left = left
    root.right = right

    val answer = searchBST(root)(3)
    assert(answer == right)
  }

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }

}
