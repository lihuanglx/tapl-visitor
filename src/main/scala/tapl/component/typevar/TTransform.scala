package tapl.component.typevar

import tapl.common._

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]] {
  override def TyVar(x: String): Exp[A] = CTyVar[A](x)
}
