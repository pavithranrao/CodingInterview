object SameTree {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }

  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
    @inline
    def dfs(a: TreeNode, b: TreeNode): Boolean = {
      if (a == null && b == null) {
        true
      } else if (a == null || b == null) {
        false
      } else {
        a.value == b.value &&
          dfs(a.left, b.left) && dfs(a.right, b.right)
      }
    }

    dfs(p, q)
  }
}
