object MaxDepth {

  // Definition for a binary tree node.
  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }

  def maxDepth(root: TreeNode): Int = {
    if (root == null) {
      0
    } else {
      (maxDepth(root.left) + 1) max (maxDepth(root.right) + 1)
    }
  }


  def main(args: Array[String]): Unit = {
    val node1 = new TreeNode(1)
    val node2 = new TreeNode(2)
    val node3 = new TreeNode(3)
    val node4 = new TreeNode(4)
    val node5 = new TreeNode(5)
    val node6 = new TreeNode(6)
    val node7 = new TreeNode(7)

    node5.left = node4
    node5.right = node6

    node6.right = node7

    node4.left = node3
    node3.left = node2
    node2.left = node1

    val answer = maxDepth(node5)
    println(answer)

  }

}
