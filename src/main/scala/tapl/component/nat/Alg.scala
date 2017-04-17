package tapl.component.nat

trait Alg[-R, E] {
  def TmZero(): E

  def TmSucc(e: R): E

  def TmPred(e: R): E

  def TmIsZero(e: R): E

  def visit(e: R): E
}
