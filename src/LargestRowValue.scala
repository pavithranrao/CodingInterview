import scala.annotation.tailrec

object LargestRowValue {

  def main(args: Array[String]): Unit = {
    val root = TreeNode(1)
    val rootLeft = TreeNode(2)
    root.left = rootLeft
    root.right = TreeNode(3)
    rootLeft.left = TreeNode(4)
    rootLeft.right = TreeNode(5)

    val answer = largestValues(root)
    println(answer)
  }

  def largestValues(root: TreeNode): List[Int] = {
    if (root == null) {
      List[Int]()
    } else {
      bfs(root).map(_.map(_.value).max)
    }
  }

  def bfs(start: TreeNode): List[List[TreeNode]] = {
    @tailrec
    def bfs_helper(vertices: List[TreeNode],
                   visited: List[List[TreeNode]]): List[List[TreeNode]] = {
      val fringe = vertices
        .flatMap { vertex => List(vertex.left, vertex.right) }
        .filterNot(_ == null)

      if (fringe.isEmpty) {
        visited
      } else {
        bfs_helper(fringe, visited :+ fringe)
      }
    }

    bfs_helper(List(start), List(List(start)))
  }

  case class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }

}
