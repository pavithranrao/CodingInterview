object EmployeeImportance {

  def main(args: Array[String]): Unit = {
    val employees = List(
      Employee(1, 5, List(2, 3)),
      Employee(2, 3, List[Int]()),
      Employee(3, 3, List[Int]()))

    val answer = getImportance(employees, id = 1)
    println(answer)
    assert(answer == 11)
  }

  def getImportance(employees: List[Employee], id: Int): Int = {
    val graph = employees.map {
      employee =>
        (employee.id, employee)
    }.toMap

    def _importanceHelper(employeeId: Int): Int = {
      val employee = graph(employeeId)
      employee.subordinates.foldLeft(employee.importance) {
        case (acc, subordinate) =>
          acc + _importanceHelper(subordinate)
      }
    }

    _importanceHelper(id)
  }

  case class Employee(id: Int, importance: Int, subordinates: List[Int])

}
