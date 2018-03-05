import scala.annotation.tailrec

object GreedyJobScheduling {

  type Job = (String, Int, Int)

  def getJobSchedule(jobs: Array[Job]): (Int, Array[Job]) = {
    val comparatorFn = (a: Job, b: Job) => a._3 > b._3
    QuickSort.genericQuickSort(0, jobs.length - 1)(jobs, comparatorFn)

    // work around is to use Map[Int, Job],
    // but is a bit complex and not suited for an easy problem,
    // hence taking a imperative approach.
    val tempOut: Array[Option[Job]] = Array.fill(jobs.length)(None)

    @tailrec
    def getFirstFreeSlot(idx: Int): Int = {
      if ((idx < 0) || tempOut(idx).isEmpty) {
        idx
      } else {
        getFirstFreeSlot(idx - 1)
      }
    }

    for (i <- jobs.indices) {
      val deadline = jobs(i)._2 - 1
      val slotIdx = getFirstFreeSlot(deadline)
      if (slotIdx >= 0)
        tempOut(slotIdx) = Some(jobs(i))
    }

    val output = tempOut.filter(_.isDefined).map(_.get)
    (output.length, output)

  }

  def main(args: Array[String]): Unit = {
    //    JobID     Deadline     Profit
    //    a         2           100
    //    b         1           19
    //    c         2           27
    //    d         1           25
    //    e         3           15
    val jobs = Array(("A", 2, 100), ("B", 1, 19),
      ("C", 2, 27), ("D", 1, 25), ("E", 3, 15))

    val (maxNumJobs, selectedJobs) = getJobSchedule(jobs)
    println(s"The maximum number of jobs that can be done are: $maxNumJobs")
    println(s"The job sequences are : ${selectedJobs.mkString(", ")}")
  }

}
