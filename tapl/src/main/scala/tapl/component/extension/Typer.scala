package tapl.component.extension

import tapl.common._
import tapl.component._
import tapl.language.tyarith
import tapl.component.extension.TAlg.Factory._
import tapl.component.typed.TAlg.Factory.TyArr
import tapl.component.top.TAlg.Factory.TyTop

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[TExp[A, Exp[B]], Type[B], Exp[B]]
    with tyarith.Typer[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B]
    with typedrecord.Typer[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B]
    with let.Typer[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  override def tmUnit(): Type[B] = TyUnit[B]()

  override def tmAscribe(e: TExp[A, Exp[B]], t: Exp[B]): Type[B] = c =>
    if (tEquals(apply(e)(c))(t)) t else typeError()

  override def tmFix(e: TExp[A, Exp[B]]): Type[B] = c => {
    apply(e)(c) match {
      case TyArr(t1, t2) if tEquals(t1)(t2) => t1
      case _ => typeError()
    }
  }

  override def tmFloat(d: Double): Type[B] = TyFloat[B]()

  override def tmString(s: String): Type[B] = TyString[B]()

  override def tmTimes(e1: TExp[A, Exp[B]], e2: TExp[A, Exp[B]]): Type[B] = c => {
    (apply(e1)(c), apply(e2)(c)) match {
      case (TyFloat(), TyFloat()) => TyFloat[B]()
      case _ => typeError()
    }
  }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with tyarith.TEquals[A] with typedrecord.TEquals[A] {

  override def tyUnit(): (Exp[A]) => Boolean = {
    case TyUnit() => true
    case _ => false
  }

  override def tyString(): (Exp[A]) => Boolean = {
    case TyString() => true
    case _ => false
  }

  override def tyFloat(): (Exp[A]) => Boolean = {
    case TyFloat() => true
    case _ => false
  }
}

trait Typer2[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends Typer[A, B]
  with tyarith.Typer2[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] with ISubtypeOf[B] {

  override def tmAscribe(e: TExp[A, Exp[B]], t: Exp[B]): Type[B] = c =>
    if (apply(e)(c)(subtypeOf)(t)) t else typeError()

  override def tmFix(e: TExp[A, Exp[B]]): Type[B] = c => {
    apply(e)(c) match {
      case TyArr(t1, t2) if t2(subtypeOf)(t1) => t2
      case _ => typeError()
    }
  }
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with tyarith.SubtypeOf[A] with typedrecord.SubtypeOf[A] {

  override def tyFloat(): Exp[A] => Boolean = {
    case TyTop() => true
    case TyFloat() => true
    case _ => false
  }

  override def tyUnit(): Exp[A] => Boolean = {
    case TyTop() => true
    case TyUnit() => true
    case _ => false
  }

  override def tyString(): Exp[A] => Boolean = {
    case TyTop() => true
    case TyString() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]]
  with tyarith.Join[A] with typedrecord.Join[A] {

  override def tyUnit(): Exp[A] => Exp[A] = directJoin(TyUnit[A](), _).getOrElse(TyTop[A]())

  override def tyFloat(): Exp[A] => Exp[A] = directJoin(TyFloat[A](), _).getOrElse(TyTop[A]())

  override def tyString(): Exp[A] => Exp[A] = directJoin(TyString[A](), _).getOrElse(TyTop[A]())
}
