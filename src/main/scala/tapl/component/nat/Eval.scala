package tapl.component.nat

import tapl.common._
import tapl.component.bool
import tapl.component.bool.Factory._

trait Eval[A[-X, Y] <: Alg[X, Y] with bool.Alg[X, Y]]
  extends Alg[Exp[A], Exp[A]] with EvalAux[A] with IsNatVal[A] {

  override def TmZero(): Exp[A] = CZero[A]()

  override def TmPred(e: Exp[A]): Exp[A] =
    if (e(isVal)) {
      isNatVal(e).getOrElse(typeError())._2
    } else {
      CPred[A](apply(e))
    }

  override def TmSucc(e: Exp[A]): Exp[A] =
    if (e(isVal)) {
      isNatVal(e).getOrElse(typeError())
      CSucc[A](e)
    } else {
      CSucc[A](apply(e))
    }

  override def TmIsZero(e: Exp[A]): Exp[A] =
    if (e(isVal)) {
      val (i, _) = isNatVal(e).getOrElse(typeError())
      if (i == 0) CTrue[A]() else CFalse[A]()
    } else {
      CIsZero[A](apply(e))
    }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with IsNatVal[A] {
  override val default: Boolean = false

  override def TmZero(): Boolean = true

  override def TmSucc(e: Exp[A]): Boolean = isNatVal(e).nonEmpty
}
