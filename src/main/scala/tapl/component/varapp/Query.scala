package tapl.component.varapp

import tapl.common.Default

trait Query[R, T] extends Alg[R, T] with Default[T] {
  override def TmVar(x: String): T = default

  override def TmApp(e1: R, e2: R): T = default
}
