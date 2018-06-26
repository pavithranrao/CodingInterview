object GraphValidTree {

  def main(args: Array[String]): Unit = {
    val edges = Array((0, 1), (1, 2), (2, 3), (1, 3), (1, 4))
    assert(!validTree(n = 5, edges))

    assert(validTree(n = 5,
      edges = Array((0, 1), (0, 2), (0, 3), (1, 4))))
  }


  def validTree(n: Int, edges: Array[(Int, Int)]): Boolean = {

    val root = Array.fill(n)(-1)

    def find(x: Int): Int = {
      if (root(x) == -1) {
        x
      } else {
        // path compression
        val rootOfX = find(root(x))
        root(x) = rootOfX
        rootOfX
      }
    }

    edges.forall {
      case (x, y) =>
        val rootOfX = find(x)
        val rootOfY = find(y)

        if (rootOfX != rootOfY) {
          root(rootOfY) = rootOfX
          true
        } else {
          false
        }
    }

  }

}
