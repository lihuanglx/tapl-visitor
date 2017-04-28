package tapl.component.nat

import tapl.common.Util._
import tapl.common.{EvalAuxiliary, Exp}
import tapl.component.bool


trait Eval[A[-X, Y] <: Alg[X, Y] with bool.Alg[X, Y], M[_]]
  extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] with IsNatVal[A] {

  override def TmZero(): M[Exp[A]] = m.point(f.TmZero())

  override def TmPred(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      isNatVal(e).getOrElse(typeError())
      m.point(f.TmPred(e))
    } else {
      m.bind(apply(e))(x => m.point(f.TmPred(x)))
    }

  override def TmSucc(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      isNatVal(e).getOrElse(typeError())
      m.point(f.TmSucc(e))
    } else {
      m.bind(apply(e))(x => m.point(f.TmSucc(x)))
    }

  override def TmIsZero(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      val x = isNatVal(e).getOrElse(typeError())
      m.point(if (x == 0) f.TmTrue() else f.TmFalse())
    } else {
      m.bind(apply(e))(x => m.point(f.TmIsZero(x)))
    }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmZero(): Boolean = true

  override def TmPred(e: Exp[A]): Boolean = apply(e)

  override def TmSucc(e: Exp[A]): Boolean = apply(e)
}
