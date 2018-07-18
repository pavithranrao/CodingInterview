import scala.annotation.tailrec

object LevelOrderTraversal {

  def main(args: Array[String]): Unit = {
    val root = TreeNode(1)
    root.left = TreeNode(2)
    root.right = TreeNode(3)

    val answer = levelOrder(root)
    println(answer)

    println(dfs(root))
  }

  def levelOrder(root: TreeNode): List[List[Int]] = {
    if (root != null) {
      bfs(root).map(_.map(_.value))
    } else {
      List[List[Int]]()
    }
  }

  def bfs(start: TreeNode): List[List[TreeNode]] = {
    @tailrec
    def bfs_helper(vertices: List[TreeNode],
                   visited: List[List[TreeNode]],
                   visitedSet: Set[TreeNode] = Set[TreeNode]()): List[List[TreeNode]] = {
      val fringe = vertices
        .flatMap { vertex => List(vertex.left, vertex.right) }.distinct
        .filterNot { child => child == null || visitedSet.contains(child) }

      if (fringe.isEmpty) {
        visited
      } else {
        bfs_helper(fringe, visited :+ fringe, visitedSet ++ fringe)
      }
    }

    bfs_helper(List(start), List(List(start)))
  }

  def dfs(start: TreeNode): List[TreeNode] = {

    def dfsHelper(vertex: TreeNode,
                  visited: List[TreeNode] = List()): List[TreeNode] = {
      if (visited.contains(vertex)) {
        visited
      }
      else {
        val neighbours = List(vertex.left, vertex.right)
          .filterNot { child => child == null || visited.contains(child) }
        neighbours.foldLeft(visited :+ vertex) {
          case (accVisited, child) =>
            dfsHelper(child, accVisited)
        }
      }
    }

    dfsHelper(start)
  }

  case class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }

}
