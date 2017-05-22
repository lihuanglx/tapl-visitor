package tapl.component.simple

import tapl.common._
import tapl.component.simple.TFactory.CTyArr
import tapl.component.top.CTyTop
import tapl.component._
import tapl.language.tyarith

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with typed.Typer[A, B]
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
  with typed.TEquals[A] with tyarith.TEquals[A] with typedrecord.TEquals[A] with typevar.TEquals[A] {

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

trait Typer2[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends Typer[A, B]
  with typed.Typer2[A, B] with tyarith.Typer2[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  override def TmAscribe(e: E3[A, Exp[B]], t: Exp[B]): Type[B] = c =>
    if (apply(e)(c)(subtypeOf)(t)) t else typeError()

  override def TmFix(e: E3[A, Exp[B]]): Type[B] = c => {
    apply(e)(c) match {
      case CTyArr(t1, t2) if t2(subtypeOf)(t1) => t2
      case _ => typeError()
    }
  }
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typed.SubtypeOf[A] with tyarith.SubtypeOf[A] with typedrecord.SubtypeOf[A] with typevar.SubtypeOf[A] {

  override def TyFloat(): Exp[A] => Boolean = {
    case CTyTop() => true
    case CTyFloat() => true
    case _ => false
  }

  override def TyUnit(): Exp[A] => Boolean = {
    case CTyTop() => true
    case CTyUnit() => true
    case _ => false
  }

  override def TyString(): Exp[A] => Boolean = {
    case CTyTop() => true
    case CTyString() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]]
  with typed.Join[A] with tyarith.Join[A] with typedrecord.Join[A] with typevar.Join[A] {

  override def TyUnit(): (Exp[A]) => Exp[A] = directJoin(CTyUnit[A](), _).getOrElse(CTyTop[A]())

  override def TyFloat(): (Exp[A]) => Exp[A] = directJoin(CTyFloat[A](), _).getOrElse(CTyTop[A]())

  override def TyString(): (Exp[A]) => Exp[A] = directJoin(CTyString[A](), _).getOrElse(CTyTop[A]())
}
