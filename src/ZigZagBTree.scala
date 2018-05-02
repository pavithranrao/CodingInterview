import scala.annotation.tailrec
import scala.collection.mutable

object ZigZagBTree {

  def main(args: Array[String]): Unit = {
    val root = TreeNode(1)
    val rootLeft = TreeNode(2)
    root.left = rootLeft
    root.right = TreeNode(3)
    rootLeft.left = TreeNode(4)
    rootLeft.right = TreeNode(5)

    val answer = zigzagLevelOrder(root)
    println(answer)
  }

  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    if (root == null) {
      List[List[Int]]()
    } else {
      bfs(root).foldLeft((List[List[Int]](), true)) {
        case ((acc, order), present) =>
          if (order) {
            (acc :+ present.map(_.value), !order)
          } else {
            (acc :+ present.map(_.value).reverse, !order)
          }
      }._1
    }
  }

  def bfs(start: TreeNode): List[List[TreeNode]] = {
    val visitedSet = mutable.Set[TreeNode]()

    @tailrec
    def bfs_helper(vertices: List[TreeNode],
                   visited: List[List[TreeNode]]): List[List[TreeNode]] = {
      vertices.foreach(visitedSet.add)
      val fringe = vertices
        .flatMap { vertex => List(vertex.left, vertex.right) }
        .filterNot { child => child == null || visitedSet.contains(child) }

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
