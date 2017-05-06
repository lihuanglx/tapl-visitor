package tapl.component.nat

import tapl.common.Util._
import tapl.common.{EvalAuxiliary, Exp}
import tapl.component.bool
import tapl.component.bool.Factory._
import tapl.component.nat.Factory._

trait Eval[A[-X, Y] <: Alg[X, Y] with bool.Alg[X, Y], M[_]]
  extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] with IsNatVal[A] {

  override def TmZero(): M[Exp[A]] = m.point(CZero[A]())

  override def TmPred(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      isNatVal(e).getOrElse(typeError())
      m.point(CPred[A](e))
    } else {
      m.bind(apply(e))(x => m.point(CPred[A](x)))
    }

  override def TmSucc(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      isNatVal(e).getOrElse(typeError())
      m.point(CSucc[A](e))
    } else {
      m.bind(apply(e))(x => m.point(CSucc[A](x)))
    }

  override def TmIsZero(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      val x = isNatVal(e).getOrElse(typeError())
      m.point(if (x == 0) CTrue[A]() else CFalse[A]())
    } else {
      m.bind(apply(e))(x => m.point(CIsZero[A](x)))
    }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmZero(): Boolean = true

  override def TmPred(e: Exp[A]): Boolean = apply(e)

  override def TmSucc(e: Exp[A]): Boolean = apply(e)
}
