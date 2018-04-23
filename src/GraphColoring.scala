import Util.Matrix

object GraphColoring {

  case class Graph(numVertices: Int, adjMatrix: Matrix[Int])

  def graphColor(graph: Graph, m: Int): Either[Array[Int], Boolean] = {
    // init : mark all vertices to have no color
    val color = Array.fill(graph.numVertices)(0)

    // Check if the present color is safe for the present vertex
    def isColorSafe(source: Int, sourceColor: Int): Boolean = {
      // Returns
      //      true  => if the source color is valid
      //      false => otherwise

      // iterate on all the vertexes
      // forall => early termination upon a clash on source and dest
      (0 until graph.numVertices).forall {
        dest =>
          // check if the source and dest are connected
          if (graph.adjMatrix(source)(dest) == 0) {
            // if not connected, do not bother checking the color
            true
          } else {
            // if connected check if the color of source and dest are not same
            color(dest) != sourceColor
          }
      }
    }

    // Get first safe color for the vertex.
    def getColor(presentVertex: Int): Option[Int] = {
      // Returns
      //      Some(color : Int) first valid color for the vertex if found,
      //      None if the vertex couldn't be colored
      (1 to m).find {
        presentColor =>
          isColorSafe(presentVertex, presentColor)
      }
    }

    // iterate on all vertices and check if a color can be assigned or not
    // forall => early termination upon a vertex not being colored
    val isColorAble = (0 until graph.numVertices).forall {
      presentVertex =>
        getColor(presentVertex) match {
          // if a valid color exists,
          // assign the color to the present vertex
          case Some(presentColor) =>
            color(presentVertex) = presentColor
            true
          // return false as a color cannot be fount for the vertex
          case _ =>
            false
        }
    }

    if (isColorAble) {
      Left(color)
    } else {
      Right(isColorAble)
    }
  }


  def main(args: Array[String]): Unit = {
    val adjMatrix = Array(
      Array(0, 1, 0, 1),
      Array(1, 0, 0, 1),
      Array(1, 0, 0, 1),
      Array(0, 1, 1, 0))

    val graph = Graph(adjMatrix.length, adjMatrix)
    val answer = graphColor(graph, 2)

    answer match {
      case Left(color) =>
        println(s"The graph is color able and the colors are : ${color.mkString(", ")}")
      case _ =>
        println(s"The graph is not color able")
    }

    // negative test case
    // this cannot be colored by 2 colors
    val invalidAdjMatrix = Array(
      Array(0, 1, 0, 1),
      Array(1, 0, 0, 1),
      Array(1, 0, 0, 1),
      Array(1, 1, 1, 0))

    val invalidGraph = Graph(invalidAdjMatrix.length, invalidAdjMatrix)
    assert(graphColor(invalidGraph, 2).isRight)
  }

}
