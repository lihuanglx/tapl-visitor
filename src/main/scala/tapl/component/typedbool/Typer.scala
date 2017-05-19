package tapl.component.typedbool

import tapl.common.Util._
import tapl.common.{Exp, TyperAuxEq, TyperAuxSub}

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends Alg[Exp[A], Type[B]] with TyperAuxEq[B] {

  override def TmTrue(): Type[B] = CTyBool[B]()

  override def TmFalse(): Type[B] = CTyBool[B]()

  protected def retTypeIf(t1: Exp[B], t2: Exp[B]): Exp[B] =
    if (t1(tEquals)(t2)) t1 else typeError()

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Type[B] = c =>
    apply(e1)(c) match {
      case CTyBool() => retTypeIf(apply(e2)(c), apply(e3)(c))
      case _ => typeError()
    }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyBool(): Exp[A] => Boolean = {
    case CTyBool() => true
    case _ => false
  }
}

trait Typer2[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends Typer[A, B] with TyperAuxSub[B] {
  override protected def retTypeIf(t1: Exp[B], t2: Exp[B]): Exp[B] = chooseSuper(t1, t2)
}
