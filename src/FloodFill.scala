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
    val negAnswer = floodFill(negImage, 0, 0, 2)
    Util.printMatrix(negAnswer)

  }

  def floodFill(image: Array[Array[Int]], sr: Int, sc: Int, newColor: Int): Array[Array[Int]] = {

    val m = image.length
    val n = image.head.length

    def dfs(row: Int = sr, col: Int = sc,
            startColor : Int = image(sr)(sc)): Unit = {
      if (!(row < 0 || row >= m || col < 0 || col >= n ||
        image(row)(col) != startColor || image(row)(col) == newColor)) {
        image(row)(col) = newColor
        dfs(row - 1, col, startColor)
        dfs(row + 1, col, startColor)
        dfs(row, col - 1, startColor)
        dfs(row, col + 1, startColor)
      }
    }

    dfs()
    image
  }

}
