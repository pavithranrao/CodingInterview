import Util.Matrix

// Courtesy : https://www.youtube.com/watch?v=oDhu5uGq_ic
object StockNTransactions {

  def getMaxProfit(stockPrice: Array[Int], maxTransactions: Int): Int = {
    // init stage
    // C[0 ... maxTransactions][0 ... (n - 1) days]
    val length = stockPrice.length
    // Base Cases

    // for zero transactions, no profit is possible
    // C[0][1 ... lastDay] = [0 ... 0]

    // for firstDay, no profit is possible
    // C[0 ... maxTransactions][0] = 0
    val C: Matrix[Int] = Array.ofDim[Int](maxTransactions + 1, length)

    // iterate with transactions from 1 to maxTransactions
    // i.e. increase transaction by one till maxTransactions
    for (numTransactions <- 1 to maxTransactions) {
      // iterate on days
      // marking the present day as selling day
      for (sellDay <- 1 until length) {
        // what if no transaction is made on selling day?
        C(numTransactions)(sellDay) = C(numTransactions)(sellDay - 1)
        // what if a transaction is made?
        // compute all possible buying and selling combinations
        for (buyDay <- 0 until sellDay) {
          // profit on the buying day with one less transaction
          val profitOnBuyDay = C(numTransactions - 1)(buyDay)
          // profit if a new transaction is made from
          // buying day to selling day
          val transactionProfit = stockPrice(sellDay) - stockPrice(buyDay)
          // net profit => sum of profitOnBuyDay and transactionProfit
          val netProfit = profitOnBuyDay + transactionProfit
          // take the max profit possible ending on selling day
          C(numTransactions)(sellDay) = C(numTransactions)(sellDay) max netProfit
        }
      }
    }

    Util.printMatrix(C)
    C(maxTransactions, length - 1)

  }

  def main(args: Array[String]): Unit = {

    val stockPrice = Array(2, 5, 7, 1, 4, 3, 1, 3)
    val maxTransactions = 3
    println(s"The stock prices are : ${stockPrice.mkString(", ")}")

    val maxProfit = getMaxProfit(stockPrice, maxTransactions)
    println(s"The maximum profit is : $maxProfit")

  }

}
