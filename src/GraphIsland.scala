object GraphIsland {

  def main(args: Array[String]): Unit = {
    val gridInput =
      """|11110
         |11010
         |11000
         |00000""".stripMargin

    val grid = getGraph(gridInput)
    val answer = numIslands(grid)
    println(answer)

    val gridInput2 =
      """|11000
         |11000
         |00100
         |00011""".stripMargin
    assert(numIslands(getGraph(gridInput2)) == 3)
  }

  def getGraph(input: String): Array[Array[Char]] = {
    input.split("\n").map(_.toCharArray)
  }
  
  def numIslands(grid: Array[Array[Char]]): Int = {
    val m = grid.length
    if (m == 0) {
      0
    } else {
      val n = grid.head.length

      def dfs(row: Int, col: Int): Int = {
        if (row < 0 || row >= m || col < 0 || col >= n || grid(row)(col) == '0') {
          0
        } else {
          grid(row)(col) = '0'
          1 +
            dfs(row - 1, col) +
            dfs(row + 1, col) +
            dfs(row, col - 1) +
            dfs(row, col + 1)
        }
      }

      val isIsland =
        for {i <- grid.indices
             j <- grid.head.indices
             if grid(i)(j) == '1'}
          yield {
            dfs(i, j)
            1
          }
      isIsland.sum
    }
  }

}
