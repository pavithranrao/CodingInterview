import scala.collection.mutable

object BipartiteGraph {

  def isBipartite(graph: Array[Array[Int]]): Boolean = {
    val color = mutable.Map[Int, Int]()

    def dfs(currentVertex: Int, currentColor: Int = 1): Boolean = {
      if (color.contains(currentVertex)) {
        color(currentVertex) == currentColor
      } else {
        color.update(currentVertex, currentColor)
        graph(currentVertex).forall {
          vertex =>
            dfs(vertex, currentColor = currentColor * -1)
        }
      }
    }

    graph.indices.forall {
      vertex =>
        color.contains(vertex) || dfs(vertex)
    }
  }

  def main(args: Array[String]): Unit = {
    val graph = Array(
      Array(1, 3),
      Array(0, 2),
      Array(1, 3),
      Array(0, 2))
    val answer = isBipartite(graph)
    if (answer) {
      println("The graph is bipartite")
    } else {
      println("The graph is not bipartite")
    }
  }

}
