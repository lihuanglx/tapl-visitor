package tapl.component.nat

import tapl.common._
import tapl.component.bool
import tapl.component.bool.Term.Factory._
import tapl.component.nat.Term._

trait Eval[A[-X, Y] <: Term[X, Y] with bool.Term[X, Y]] extends Term[Exp[A], Exp[A]] with IIsVal[A] {
  override def tmZero(): Exp[A] = TmZero[A]()

  override def tmPred(e: Exp[A]): Exp[A] =
    e match {
      case TmZero() => e
      case TmSucc(x) => x
      case _ => if (e(isVal)) typeError() else TmPred[A](apply(e))
    }

  override def tmSucc(e: Exp[A]): Exp[A] =
    if (e(isVal))
      e match {
        case TmZero() => TmSucc[A](e)
        case TmSucc(_) => TmSucc[A](e)
        case _ => typeError()
      }
    else
      TmSucc[A](apply(e))


  override def tmIsZero(e: Exp[A]): Exp[A] =
    if (e(isVal))
      e match {
        case TmZero() => TmTrue[A]()
        case TmSucc(_) => TmFalse[A]()
        case _ => typeError()
      }
    else
      TmIsZero[A](apply(e))
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with IsNatVal[A] {
  override val default: Boolean = false

  override def tmZero(): Boolean = true

  override def tmSucc(e: Exp[A]): Boolean = isNatVal(e).isDefined
}
