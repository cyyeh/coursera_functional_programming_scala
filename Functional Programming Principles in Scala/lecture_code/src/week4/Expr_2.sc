import week4.{Expr_2, Number_2, Sum_2}

object Expr_2 {
  def show(e: Expr_2): String = e match {
    case Number_2(x) => x.toString
    case Sum_2(l, r) => show(l) + " + " + show(r)
  }

  show(Sum_2(Number_2(1), Number_2(44)))
}