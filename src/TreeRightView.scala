import scala.annotation.tailrec
import scala.collection.mutable

object TreeRightView {


  def main(args: Array[String]): Unit = {
    val root = TreeNode(1)
    root.left = TreeNode(2)
    root.right = TreeNode(3)

    val answer = rightSideView(root)
    println(answer)
    assert(answer == List(1, 3))

    val root2: TreeNode = null
    assert(rightSideView(root2) == List[Int]())
  }

  def rightSideView(root: TreeNode): List[Int] = {
    if (root != null) {
      bfs(root).map(_.last.value)
    } else {
      List[Int]()
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
