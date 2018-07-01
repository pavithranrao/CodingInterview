object GreedyActivitySelection {

  type Activity = (Int, Int) // Start, End

  def getActivities(activities: Array[Activity]): Array[Activity] = {
    val comparatorFn = (a: Activity, b: Activity) => a._2 < b._2
    QuickSort.genericQuickSort(0, activities.length - 1)(activities, comparatorFn)

    val selActivities = activities.tail.foldLeft(Array(activities.head)) {
      case (acc, present) =>
        if (acc.last._2 < present._1) {
          acc :+ present
        } else {
          acc
        }
    }

    selActivities
  }

  def main(args: Array[String]): Unit = {
    val activities: Array[Activity] = Array((1, 2), (5, 7), (8, 9), (3, 4), (0, 6), (5, 9))
    println(s"The given activities are : ${activities.mkString(", ")}")


    val selActivities = getActivities(activities)
    println(s"The maximum no of activities are : ${selActivities.length}")
    println(s"The selected activities are : ${selActivities.mkString(", ")}")
  }

}
