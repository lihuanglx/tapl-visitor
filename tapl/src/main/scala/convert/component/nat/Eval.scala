package convert.component.nat

import convert.common._
import convert.component.bool
import convert.component.bool.Term.Factory._
import convert.component.nat.Term._

trait Eval[A[-X, Y] <: Term[X, Y] with bool.Term[X, Y]]
  extends Term[Exp[A], Exp[A]] with IIsVal[A] with IsNatVal[A] {

  override def tmZero(): Exp[A] = TmZero[A, A]()

  override def tmPred(e: Exp[A]): Exp[A] =
    if (e(isVal)) {
      isNatVal(e).getOrElse(typeError())._2
    } else {
      TmPred[A, A](apply(e))
    }

  override def tmSucc(e: Exp[A]): Exp[A] =
    if (e(isVal)) {
      isNatVal(e).getOrElse(typeError())
      TmSucc[A, A](e)
    } else {
      TmSucc[A, A](apply(e))
    }

  override def tmIsZero(e: Exp[A]): Exp[A] =
    if (e(isVal)) {
      val (i, _) = isNatVal(e).getOrElse(typeError())
      if (i == 0) TmTrue[A, A]() else TmFalse[A, A]()
    } else {
      TmIsZero[A, A](apply(e))
    }
}

trait IsVal[A[-X, Y] <: Term[X, Y]] extends Query[Exp[A], Boolean] with IsNatVal[A] {
  override def default: Boolean = false

  override def tmZero(): Boolean = true

  override def tmSucc(e: Exp[A]): Boolean = isNatVal(e).nonEmpty
}
