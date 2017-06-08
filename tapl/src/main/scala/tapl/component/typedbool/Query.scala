package tapl.component.typedbool

import tapl.common._
import tapl.component.bool

trait Query[R, T] extends Alg[R, T] with bool.Alg.Query[R, T]

trait TQuery[R, T] extends TAlg[R, T] with Default[T] {
  override def TyBool(): T = default
}
