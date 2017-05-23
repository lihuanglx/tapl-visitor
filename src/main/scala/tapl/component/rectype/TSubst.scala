package tapl.component.rectype

import tapl.common._

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TTransform[A] with SubstAux[A] {
  override def TyRec(x: String, t: Exp[A]): Exp[A] = CTyRec[A](x, if (x == this.x) t else apply(t))
}

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]] {
  override def TyRec(x: String, t: Exp[A]): Exp[A] = CTyRec[A](x, apply(t))
}
