package tapl.component.varapp

trait Alg[-R, E] {
  def TmVar(x: String): E

  def TmApp(e1: R, e2: R): E

  def apply(e: R): E
}
