package week4

// suppose all you want to do is evaluate expressions

trait Expr_1 {
  def eval: Int
}

class Number_1(n: Int) extends Expr_1 {
  def eval: Int = n
}

class Sum_1(e1: Expr_1, e2: Expr_1) extends Expr_1 {
  def eval: Int = e1.eval + e2.eval
}
