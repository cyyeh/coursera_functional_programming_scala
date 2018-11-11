import week4.List

object list {
  List.apply(1, 2).head
  List(1, 2).head
  List(1, 2).tail.head

  val f = new Function1[Int, Int] {
    def apply(x: Int) = x * x
  }
  f.apply(2)
  f(3)
}