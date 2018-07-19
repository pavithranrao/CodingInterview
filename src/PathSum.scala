object PathSum {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }

  def hasPathSum(root: TreeNode, sum: Int): Boolean = {
    @inline
    def isLeaf(treeNode: TreeNode): Boolean = {
      treeNode.left == null && treeNode.right == null
    }

    def dfs(presentNode: TreeNode, accSum: Int = 0): Boolean = {
      if (presentNode != null) {
        if (isLeaf(presentNode)) {
          presentNode.value + accSum == sum
        } else {
          dfs(presentNode.left, accSum + presentNode.value) ||
            dfs(presentNode.right, accSum + presentNode.value)
        }
      } else {
        false
      }
    }

    dfs(root)
  }

  def main(args: Array[String]): Unit = {
    val treeNode = new TreeNode(1)
    treeNode.left = new TreeNode(2)

    val answer = hasPathSum(treeNode, 1)
    println(answer)
  }

}
