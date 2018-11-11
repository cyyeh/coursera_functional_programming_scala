package week4

trait Expr_2 {
  def eval: Int = this match {
    case Number_2(n) => n
    case Sum_2(e1, e2) => e1.eval + e2.eval
  }
}
case class Number_2(n: Int) extends Expr_2
case class Sum_2(e1: Expr_2, e2: Expr_2) extends Expr_2
