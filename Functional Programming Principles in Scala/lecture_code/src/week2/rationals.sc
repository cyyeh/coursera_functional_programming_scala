object rationals {
  val x = new Rational(1, 2)
  val y = new Rational(2, 3)

  x.add(y).sub(x)
}

class Rational(x: Int, y: Int) {
  // precondition
  require(y > 0, "denominator must be positive")

  // second usage of constructor
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  val numer = x / g
  val denom = y / g

  def less(that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this.less(that)) that else this

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg: Rational = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  override def toString = numer + "/" + denom
}
