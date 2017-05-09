package tapl.component.nat

import tapl.common.Util._
import tapl.common.{EvalAux, Exp}
import tapl.component.bool
import tapl.component.bool.Factory._
import tapl.component.nat.Factory._

import scalaz.Scalaz._

trait Eval[A[-X, Y] <: Alg[X, Y] with bool.Alg[X, Y], M[_]]
  extends Alg[Exp[A], M[Exp[A]]] with EvalAux[A, M] with IsNatVal[A] {

  override def TmZero(): M[Exp[A]] = m.point(CZero[A]())

  override def TmPred(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      val (_, p) = isNatVal(e).getOrElse(typeError())
      m.point(p)
    } else for {
      _e <- apply(e)
    } yield CPred[A](_e)

  override def TmSucc(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      isNatVal(e).getOrElse(typeError())
      m.point(CSucc[A](e))
    } else for {
      _e <- apply(e)
    } yield CSucc[A](_e)

  override def TmIsZero(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      val (i, _) = isNatVal(e).getOrElse(typeError())
      m.point(if (i == 0) CTrue[A]() else CFalse[A]())
    } else for {
      _e <- apply(e)
    } yield CIsZero[A](_e)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with IsNatVal[A] {
  override val default: Boolean = false

  override def TmZero(): Boolean = true

  override def TmSucc(e: Exp[A]): Boolean = isNatVal(e).nonEmpty
}
