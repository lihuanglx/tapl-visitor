package tapl.component.let

import tapl.common.Default

trait Query[R, T] extends Alg[R, T] with Default[T] {
  override def TmLet(x: String, e1: R, e2: R): T = default
}
