import scala.annotation.tailrec
import scala.collection.mutable

object BottomLeftMost {

  case class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }


  def findBottomLeftValue(root: TreeNode): Int = {
    def bfs(start: TreeNode): List[List[TreeNode]] = {
      val visitedSet = mutable.Set[TreeNode]()

      @tailrec
      def bfs_helper(vertices: List[TreeNode],
                     visited: List[List[TreeNode]]): List[List[TreeNode]] = {
        vertices.foreach(visitedSet.add)
        val fringe = vertices
          .flatMap { vertex => List(vertex.left, vertex.right) }
          .filter(child => child != null && !visitedSet.contains(child))

        if (fringe.isEmpty)
          visited
        else
          bfs_helper(fringe, fringe :: visited)
      }

      bfs_helper(List(start), List(List(start))).reverse
    }

    bfs(root).last.head.value
  }


  def main(args: Array[String]): Unit = {
    val root = TreeNode(1)
    root.left = TreeNode(2)
    root.right = TreeNode(3)

    val answer = findBottomLeftValue(root)
    println(answer)
    assert(answer == 2)

  }

}
