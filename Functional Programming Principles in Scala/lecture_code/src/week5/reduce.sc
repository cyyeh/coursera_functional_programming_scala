object reduce {
  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sum(ys)
  }

  def product(xs: List[Int]): Int = (1 :: xs) reduceLeft (_ * _)
  def product2(xs: List[Int]): Int = (xs foldLeft 1) (_ * _)

  def concat(xs: List[Int], ys: List[Int]): List[Int] =
    (xs foldRight ys)(_ :: _)
}
