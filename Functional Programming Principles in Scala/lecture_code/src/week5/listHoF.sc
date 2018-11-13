object listHoF {
  def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
    case Nil => xs
    case y :: ys => y * factor :: scaleList(ys, factor)
  }

  scaleList(List(1, 2, 3), 2.5)

  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys)
  }

  squareList(List(1, 2, 3))

  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case x :: xs => if (x > 0) x :: posElems(xs) else posElems(xs)
  }

  posElems(List(-1, 1, -2, 3, 0))

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }

  pack(List('a', 'a', 'b', 'c', 'c', 'a'))

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.length))

  encode(List('a', 'a', 'b', 'c', 'c', 'a'))
}