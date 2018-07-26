import scala.annotation.tailrec

object AverageOfLevels {

  def main(args: Array[String]): Unit = {
    val root = TreeNode(1)
    root.left = TreeNode(2)
    root.right = TreeNode(3)

    val answer = averageOfLevels(root)
    println(answer.mkString(", "))
  }

  def averageOfLevels(root: TreeNode): Array[Double] = {
    if (root != null) {
      bfs(root).map {
        level =>
          Util.average(level.map(_.value))
      }.toArray
    } else {
      Array[Double]()
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
