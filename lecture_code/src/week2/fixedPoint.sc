/**
  * A number x is called a fixed point of a function if f(x) = x
  */

import math.abs

object fixedPoint {
  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double): Boolean =
    abs((x - y) / x) / x < tolerance
  def findFixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }

  findFixedPoint(x => 1 + x/2)(1)

  /**
    * sqrt(x) is a fixed point of the function (y => x / y)
    * trick: average damping, in order to prevent the estimation from
    * varying to much. This is done by averaging successive values of
    * the original sequence
    */
  def sqrt(x: Double): Double = findFixedPoint(y => (y + x / y) / 2)(1.0)

  sqrt(2)

  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2
  def newSqrt(x: Double): Double =
    findFixedPoint(averageDamp(y => x / y))(1.0)

  newSqrt(2)
}