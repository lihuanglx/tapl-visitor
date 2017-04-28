package tapl.component.floatstring

import tapl.common.Default

trait Query[R, T] extends Alg[R, T] with Default[T] {
  override def TmFloat(d: Double): T = default

  override def TmString(s: String): T = default

  override def TmTimes(e1: R, e2: R): T = default
}
