object UniValuePath {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }

  def longestUnivaluePath(root: TreeNode): Int = {

    def _recursive(node: TreeNode, value: Int): Int = {
      if (node == null || node.value != value) {
        0
      } else {
        1 + _recursive(node.left, value) max _recursive(node.right, value)
      }
    }

    if (root == null) {
      0
    } else {
      val sub = longestUnivaluePath(root.left) max longestUnivaluePath(root.right)
      sub + _recursive(root.left, root.value) max _recursive(root.right, root.value)
    }

  }

  def main(args: Array[String]): Unit = {

  }

}
