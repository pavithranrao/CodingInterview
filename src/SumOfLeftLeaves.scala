object SumOfLeftLeaves {

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(2)
    val left = new TreeNode(1)
    val right = new TreeNode(3)

    root.left = left
    root.right = right

    val answer = sumOfLeftLeaves(root)
    assert(answer == 2)
  }

  def sumOfLeftLeaves(root: TreeNode): Int = {
    def helper(node: TreeNode, acc: Int = 0): Int = {
      if (node == null) {
        acc
      } else {
        if (node.left != null && node.left.left == null && node.left.right == null) {
          helper(node.right, acc + node.left.value)
        } else {
          val newAcc = helper(node.left, acc)
          helper(node.right, newAcc)
        }
      }
    }

    helper(root)
  }

  def sumOfLeftLeaves2(root: TreeNode): Int = {
    def sumLeft(node: TreeNode, isLeft: Boolean = false): Int = {
      if (node == null) {
        0
      } else if (node.left == null && node.right == null) {
        if (isLeft)
          node.value
        else
          0
      } else {
        sumLeft(node.left, isLeft = true) + sumLeft(node.right)
      }
    }

    sumLeft(root)
  }


  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }


}
