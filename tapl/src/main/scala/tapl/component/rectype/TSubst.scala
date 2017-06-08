package tapl.component.rectype

import tapl.common._
import tapl.component.rectype.TAlg.Factory._

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TTransform[A] with SubstAux[A] {
  override def tyRec(x: String, t: Exp[A]): Exp[A] = TyRec[A](x, if (x == this.x) t else apply(t))
}

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]] {
  override def tyRec(x: String, t: Exp[A]): Exp[A] = TyRec[A](x, apply(t))
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with IRecEq[A] {
  override def tyRec(x: String, t: Exp[A]): Exp[A] => Boolean = recEq.tyRec(x, t)(Set.empty)
}

trait RecEq[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean]
  with ISubst[A] {

  override def tyRec(x: String, t: Exp[A]): Set[(Exp[A], Exp[A])] => Exp[A] => Boolean =
    c => u => {
      val p = (TyRec(x, t), u)
      c(p) || {
        val s = t(subst(x, p._1))
        apply(s)(c + p)(u)
      }
    }
}
