package tapl.component.lambda

import tapl.common.Default

trait Query[R, T] extends Alg[R, T] with Default[T] {
  override def TmAbs(x: String, e: R): T = default
}
