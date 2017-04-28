package tapl.component.nat

import tapl.common.Default

trait Query[R, T] extends Alg[R, T] with Default[T] {
  override def TmZero(): T = default

  override def TmPred(e: R): T = default

  override def TmSucc(e: R): T = default

  override def TmIsZero(e: R): T = default
}
