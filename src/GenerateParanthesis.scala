object GenerateParanthesis {
    def generateParenthesis(n: Int): List[String] = {
      if (n == 0){
        List[String]()
      } else {
        def helper(buffer : String, numAvailable : Int, numUnClosed : Int) : List[String] = {
          if (numAvailable > 0) {
            if (numAvailable == n || numUnClosed == 0) {
              helper(s"$buffer(", numAvailable - 1, numUnClosed + 1)
            } else {
              helper(s"$buffer(", numAvailable - 1, numUnClosed + 1) ++
              helper(s"$buffer)", numAvailable, numUnClosed - 1)
            }
          } else {
            val closure = ")" * numUnClosed
            List(s"$buffer$closure")
          }
        }

        helper("", n, 0)
      }

    }
}
