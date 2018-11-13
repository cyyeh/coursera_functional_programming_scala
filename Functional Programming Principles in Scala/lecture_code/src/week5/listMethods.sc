object listMethods {
  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => reverse(ys) ++ List(y)
  }

  def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)

  def flatten(xs: List[Any]): List[Any] = {
    def _flatten(res: List[Any], rem: List[Any]):List[Any] = rem match {
      case Nil => res
      case (h:List[_])::Nil => _flatten(res, h)
      case (h:List[_])::tail => _flatten(res:::h, tail)
      case h::tail => _flatten(res:::List(h), tail)
    }
    _flatten(List(), xs)
  }

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
}