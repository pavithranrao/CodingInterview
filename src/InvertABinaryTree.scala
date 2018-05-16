object InvertABinaryTree {

  def main(args: Array[String]): Unit = {
    val root = TreeNode(1)
    root.left = TreeNode(2)
    root.right = TreeNode(3)

    invertTree(root)
  }

  def invertTree(node: TreeNode): TreeNode = {
    if (node == null) {
      // Base Case:
      //   cannot invert a node which is null
      //   return the same node
      node
    } else {
      // invert subtrees
      val invertedLeft = invertTree(node.left)
      val invertedRight = invertTree(node.right)

      // assign inverted subtrees
      node.left = invertedRight
      node.right = invertedLeft

      // return the root with inverted subtrees
      node
    }
  }

  def invertTree2(node: TreeNode): TreeNode = {
    if (node == null) {
      // Base Case:
      //   cannot invert a node which is null
      //   return the same node
      node
    } else {
      val temp = node.left

      node.left = node.right
      node.right = temp
      invertTree2(node.left)
      invertTree2(node.right)

      // return the root with inverted subtrees
      node
    }
  }

  case class TreeNode(value: Int) {
    var left: TreeNode = _
    var right: TreeNode = _
  }

}
