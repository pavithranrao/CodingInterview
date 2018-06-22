object GradeProb {

  //  https://www.hackerrank.com/challenges/grading/problem
  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    for (_ <- 0 until n) {
      val grade = sc.nextInt()
      if (grade >= 38) {
        (grade % 5).compare(2) match {
          case 1 =>
            println(grade + (5 - grade % 5))
          case _ =>
            println(grade)
        }
      } else {
        println(grade)
      }
    }

  }
}
