object MaximumSumPath {
  def main(args: Array[String]): Unit = {
    val root = TreeNode(-25)
    root.left = TreeNode(15)
    val right = TreeNode(30)
    root.right = right
    right.left = TreeNode(10)
    right.right = TreeNode(20)

    val answer = getMaximumSumPath(root)
    println(answer.get)

    // negative test cases
    val negRoot = TreeNode(-10)
    negRoot.left = TreeNode(-10)
    negRoot.right = TreeNode(-10)
    assert(getMaximumSumPath(negRoot).get == -10)

    val negRoot2 = TreeNode(-10)
    negRoot2.left = TreeNode(-20)

    assert(getMaximumSumPath(negRoot2).get == -10)

    val negRoot3 = TreeNode(10)
    negRoot3.left = TreeNode(20)

    assert(getMaximumSumPath(negRoot3).get == 30)

    val negRoot4: TreeNode = null
    assert(getMaximumSumPath(negRoot4).isEmpty)

  }

  //  def getMaximumSumPath(root: TreeNode): Int = {
  //
  //    def isLeaf(node: TreeNode): Boolean =
  //      node.left == null && node.right == null
  //
  //    // revisit
  //    // must make it mode concise
  //    def maxSumPathHelper(node: TreeNode): Int = {
  //      if (isLeaf(node)) {
  //        node.value
  //      } else if (node.left == null && isLeaf(node.right)) {
  //        val right = maxSumPathHelper(node.right)
  //        node.value max right max (node.value + right)
  //      } else if (node.right == null && isLeaf(node.left)) {
  //        val left = maxSumPathHelper(node.left)
  //        node.value max left max (node.value + left)
  //      } else {
  //        val left = maxSumPathHelper(node.left)
  //        val right = maxSumPathHelper(node.right)
  //        //  println(s">>>>\n$node")
  //        //  println(left)
  //        //  println(right)
  //
  //        val maxSingle = ((left max right) + node.value) max node.value max left max right
  //        val maxTop = maxSingle max (left + right + node.value)
  //        //  println(maxSingle)
  //        //  println(maxTop)
  //        maxTop
  //
  //      }
  //    }
  //
  //    maxSumPathHelper(root)
  //  }

  def getMaximumSumPath(root: TreeNode): Option[Int] = {

    def maxSumPathHelper(node: TreeNode): Option[Int] = {
      if (node == null) {
        None
      }
      else {
        val left = maxSumPathHelper(node.left)
        val right = maxSumPathHelper(node.right)

        (left.isDefined, right.isDefined) match {
          case (false, false) =>
            Some(node.value)
          case (true, true) =>
            val leftMax = left.get
            val rightMax = right.get
            val maxSingle = ((leftMax max rightMax) + node.value) max
              node.value max leftMax max rightMax
            val maxTop = maxSingle max (leftMax + rightMax + node.value)
            Some(maxTop)
          case (_, true) =>
            Some(node.value max right.get max (node.value + right.get))
          case (true, _) =>
            Some(node.value max left.get max (node.value + left.get))
        }
      }
    }

    if (root != null) {
      maxSumPathHelper(root)
    } else {
      None
    }
  }

  case class TreeNode(value: Int) {
    var left: TreeNode = _
    var right: TreeNode = _
  }

}
