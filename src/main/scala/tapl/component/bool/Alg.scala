package tapl.component.bool

trait Alg[-R, E] {
  def TmTrue(): E

  def TmFalse(): E

  def TmIf(e1: R, e2: R, e3: R): E

  def apply(e: R): E
}