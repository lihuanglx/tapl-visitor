package tapl.component.rectype

import tapl.common._

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TTransform[A] with SubstAux[A] {
  override def TyRec(x: String, t: Exp[A]): Exp[A] = CTyRec[A](x, if (x == this.x) t else apply(t))
}

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]] {
  override def TyRec(x: String, t: Exp[A]): Exp[A] = CTyRec[A](x, apply(t))
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with IRecEq[A] {
  override def TyRec(x: String, t: Exp[A]): Exp[A] => Boolean = recEq.TyRec(x, t)(Set.empty)
}

trait RecEq[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean]
  with ISubst[A] {

  override def TyRec(x: String, t: Exp[A]): Set[(Exp[A], Exp[A])] => Exp[A] => Boolean =
    c => u => {
      val p = (CTyRec(x, t), u)
      c(p) || {
        val s = t(subst(x, p._1))
        apply(s)(c + p)(u)
      }
    }
}
