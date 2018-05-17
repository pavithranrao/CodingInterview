object PruneTree {
  def main(args: Array[String]): Unit = {

    val root = TreeNode(1)
    val left = TreeNode(0)
    root.left = left
    left.left = TreeNode(1)
    left.right = TreeNode(0)

    val prunedTree = pruneTree(root)

    // pruned leaf node which had `0`
    assert(prunedTree.left.right == null)

    // negative test case
    assert(pruneTree(TreeNode(0)) == null)
  }

  def pruneTree(root: TreeNode): TreeNode = {
    def containsOne(node: TreeNode): Boolean = {
      if (node == null) {
        // Base Case
        //   remove the node that doesn't contain `1`
        false
      } else {
        val leftContainsOne = containsOne(node.left)
        val rightContainsOne = containsOne(node.right)

        // remove the subtree that doesn't contain `1`
        if (!leftContainsOne)
          node.left = null
        if (!rightContainsOne)
          node.right = null

        // for a subtree to be present in the pruned tree
        //   either the value must be `1` (early termination) OR
        //   one of the subtrees (left OR right) must have a `1`
        node.value == 1 || leftContainsOne || rightContainsOne
      }
    }

    if (containsOne(root)) {
      root
    }
    else {
      // negative case
      // entire tree has no `1`
      null
    }
  }

  case class TreeNode(value: Int) {
    var left: TreeNode = _
    var right: TreeNode = _
  }

}
