import scala.annotation.tailrec

object DeleteDuplicates {

  def main(args: Array[String]): Unit = {
    val head = new ListNode(1)
    val second = new ListNode(1)
    second.next = new ListNode(2)
    head.next = second

    deleteDuplicates(head)

    assert(head.next.x == 2)
  }

  // beats 100%
  def deleteDuplicates(head: ListNode): ListNode = {
    @tailrec
    def deleteHelper(present: ListNode = head.next,
                     prev: ListNode = head): Unit = {
      if (present != null) {
        if (present.x == prev.x) {
          prev.next = present.next
          deleteHelper(present = present.next, prev = prev)
        } else {
          deleteHelper(present = present.next, prev = present)
        }
      }
    }

    if (head != null && head.next != null)
      deleteHelper()

    head
  }

  class ListNode(var _x: Int = 0) {
    var next: ListNode = _
    var x: Int = _x
  }

}
