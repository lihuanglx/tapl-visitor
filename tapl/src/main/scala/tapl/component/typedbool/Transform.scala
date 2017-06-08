package tapl.component.typedbool

import tapl.common.Exp
import tapl.component.bool

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with bool.Transform[A]

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]] {
  override def TyBool(): Exp[A] = CTyBool[A]()
}
