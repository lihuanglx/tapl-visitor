package tapl.component.simple

import tapl.common.Exp
import tapl.common.Util._
import tapl.component.simple.TFactory.CTyArr
import tapl.component.{let, typed2, typedrecord}
import tapl.language.tyarith

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with typed2.Typer[A, B]
    with tyarith.Typer[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B]
    with typedrecord.Typer[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B]
    with let.Typer[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  override def TmUnit(): Type[B] = CTyUnit[B]()

  override def TmAscribe(e: E3[A, Exp[B]], t: Exp[B]): Type[B] = c =>
    if (apply(e)(c)(tEquals)(t)) t else typeError()

  override def TmFix(e: E3[A, Exp[B]]): Type[B] = c => {
    apply(e)(c) match {
      case CTyArr(t1, t2) if t1(tEquals)(t2) => t1
      case _ => typeError()
    }
  }

  // todo
  override def TmInert(t: Exp[B]): Type[B] = ???

  override def TmFloat(d: Double): Type[B] = CTyFloat[B]()

  override def TmString(s: String): Type[B] = CTyString[B]()

  override def TmTimes(e1: E3[A, Exp[B]], e2: E3[A, Exp[B]]): Type[B] = c => {
    (apply(e1)(c), apply(e2)(c)) match {
      case (CTyFloat(), CTyFloat()) => CTyFloat[B]()
      case _ => typeError()
    }
  }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typed2.TEquals[A] with tyarith.TEquals[A] with typedrecord.TEquals[A] {

  override def TyUnit(): (Exp[A]) => Boolean = {
    case CTyUnit() => true
    case _ => false
  }

  override def TyString(): (Exp[A]) => Boolean = {
    case CTyString() => true
    case _ => false
  }

  override def TyFloat(): (Exp[A]) => Boolean = {
    case CTyFloat() => true
    case _ => false
  }
}
