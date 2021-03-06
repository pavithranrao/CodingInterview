import scala.annotation.tailrec

object BottomLeftMost {

  def main(args: Array[String]): Unit = {
    val root = TreeNode(1)
    root.left = TreeNode(2)
    root.right = TreeNode(3)

    val answer = findBottomLeftValue(root)
    println(answer)
    assert(answer == 2)

  }

  def findBottomLeftValue(root: TreeNode): Int = {
    bfs(root).last.head.value
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
