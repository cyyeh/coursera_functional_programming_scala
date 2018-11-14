object otherCollections {
  val v = Vector(1, 2, 3)
  val a = Array(1, 2, 3)
  val s = "Hello World"
  val r = 1 until 5 by 2
  val r1 = 1 to 10 by 3

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1 * xy._2).sum

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map { case (x, y) => x * y }.sum

  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
}