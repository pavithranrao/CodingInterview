object FloodFill {

  def main(args: Array[String]): Unit = {
    val image = Array(Array(1, 1, 1), Array(1, 1, 0), Array(1, 0, 1))
    val sr = 1
    val sc = 1
    val newColor = 2

    val answer = floodFill(image, sr, sc, newColor)
    Util.printMatrix(answer)

    println()

    val negImage = Array(Array(0, 0, 0), Array(0, 0, 0), Array(0, 0, 0))
    val negAnswer = floodFill(negImage, sr = 0, sc = 0, newColor)
    Util.printMatrix(negAnswer)

  }

  // beats 80%
  def floodFill(image: Array[Array[Int]], sr: Int, sc: Int, newColor: Int): Array[Array[Int]] = {

    val m = image.length
    val n = image.head.length
    val startColor = image(sr)(sc)

    def dfs(row: Int = sr, col: Int = sc): Unit = {
      if (!(row < 0 || row >= m || col < 0 || col >= n ||
        image(row)(col) != startColor || image(row)(col) == newColor)) {
        image(row)(col) = newColor
        dfs(row - 1, col)
        dfs(row + 1, col)
        dfs(row, col - 1)
        dfs(row, col + 1)
      }
    }

    dfs()
    image
  }

}
