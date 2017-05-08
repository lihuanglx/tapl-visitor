package tapl.component.typednat

import tapl.common.Default
import tapl.component.nat

trait Query[R, T] extends Alg[R, T] with nat.Query[R, T]

trait TQuery[R, T] extends TAlg[R, T] with Default[T] {
  override def TyNat(): T = default
}

