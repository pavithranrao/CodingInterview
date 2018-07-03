object FlattenList {

  def main(args: Array[String]): Unit = {

    def customFlatMap[A, B](list: Seq[A])(f: A => Seq[B]): Seq[B] =
      list.foldLeft(Seq[B]())(_ ++ f(_))


    val x = (1 to 10).toList.map {
      i => (1 to i).toList
    }

    val sumFn = (x: List[Int]) => {
      List(x.sum)
    }
    val answer = customFlatMap(x)(sumFn)
    println(answer.mkString(", "))
  }

}
