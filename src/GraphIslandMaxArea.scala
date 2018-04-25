object GraphIslandMaxArea {

  def main(args: Array[String]): Unit = {
    val grid = Array(
      Array(0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0),
      Array(0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0),
      Array(0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0))

    val answer = maxAreaOfIsland(grid)
    println(answer)

    // negative test case
    assert(maxAreaOfIsland(Array(Array(0))) == 0)
  }

  def maxAreaOfIsland(grid: Array[Array[Int]]): Int = {
    val m = grid.length
    val n = grid.head.length

    def dfs(row: Int, col: Int): Int = {
      if (row < 0 || row >= m || col < 0 || col >= n || grid(row)(col) == 0) {
        0
      } else {
        grid(row)(col) = 0
        1 +
          dfs(row - 1, col) +
          dfs(row + 1, col) +
          dfs(row, col - 1) +
          dfs(row, col + 1)
      }
    }

    val islandAreas =
      for {i <- grid.indices
           j <- grid.head.indices
           if grid(i)(j) == 1}
        yield dfs(i, j)

    if (islandAreas.isEmpty) {
      0
    } else {
      islandAreas.max
    }
  }

}
