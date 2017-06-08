package tapl.component.nat

import tapl.common._
import tapl.component.bool
import tapl.component.bool.Alg.Factory._
import tapl.component.nat.Alg._

trait Eval[A[-X, Y] <: Alg[X, Y] with bool.Alg[X, Y]]
  extends Alg[Exp[A], Exp[A]] with IIsVal[A] with IsNatVal[A] {

  override def tmZero(): Exp[A] = TmZero[A]()

  override def tmPred(e: Exp[A]): Exp[A] =
    if (e(isVal)) {
      isNatVal(e).getOrElse(typeError())._2
    } else {
      TmPred[A](apply(e))
    }

  override def tmSucc(e: Exp[A]): Exp[A] =
    if (e(isVal)) {
      isNatVal(e).getOrElse(typeError())
      TmSucc[A](e)
    } else {
      TmSucc[A](apply(e))
    }

  override def tmIsZero(e: Exp[A]): Exp[A] =
    if (e(isVal)) {
      val (i, _) = isNatVal(e).getOrElse(typeError())
      if (i == 0) TmTrue[A]() else TmFalse[A]()
    } else {
      TmIsZero[A](apply(e))
    }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with IsNatVal[A] {
  override val default: Boolean = false

  override def tmZero(): Boolean = true

  override def tmSucc(e: Exp[A]): Boolean = isNatVal(e).nonEmpty
}
