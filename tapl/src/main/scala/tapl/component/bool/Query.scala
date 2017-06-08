package tapl.component.bool

import tapl.common._

trait Query[R, T] extends Alg[R, T] with Default[T] {
  override def TmTrue(): T = default

  override def TmFalse(): T = default

  override def TmIf(e1: R, e2: R, e3: R): T = default
}
