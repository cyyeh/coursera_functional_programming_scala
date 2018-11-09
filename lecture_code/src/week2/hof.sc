object hof {
  def sumHoF(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }

    loop(a, 0)
  }

  def sumInts(a: Int, b: Int): Int = sumHoF(x => x, a, b)
  def sumCubes(a: Int, b: Int): Int = sumHoF(x => x * x * x, a, b)
  def sumFactorials(a: Int, b: Int): Int = sumHoF(fact, a, b)

  def fact(x: Int): Int = if (x == 0) 1 else fact(x - 1)

  sumInts(5, 10)
  sumCubes(5, 10)
  sumFactorials(5, 10)

  def productCurry(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * productCurry(f)(a + 1, b)

  productCurry(x => x * x)(3, 4)

  def sumCurry(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumCurry(f)(a + 1, b)

  sumCurry(x => x * x)(3, 4)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  def sum(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)
  def product(f: Int => Int)(a: Int, b: Int):Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  sum(x => x * x)(3, 4)
  product(x => x * x)(3, 4)

  mapReduce(x => x * x, (a, b) => a + b, 0)(3, 4)
  mapReduce(x => x * x, (a, b) => a * b, 1)(3, 4)
}