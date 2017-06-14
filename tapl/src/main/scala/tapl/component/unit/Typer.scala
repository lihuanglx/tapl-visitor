package tapl.component.unit

import tapl.common._
import tapl.component.top
import tapl.component.top.TAlg.Factory._
import tapl.component.unit.TAlg.Factory._

trait Typer[A[-R, E] <: Alg[R, E], B[-X, Y] <: TAlg[X, Y]] extends Alg[Exp[A], Exp[B]] {
  override def tmUnit(): Exp[B] = TyUnit[B]()
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyUnit(): (Exp[A]) => Boolean = {
    case TyUnit() => true
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyUnit(): Exp[A] => Boolean = {
    case TyTop() => true
    case TyUnit() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def tyUnit(): Exp[A] => Exp[A] = directJoin(TyUnit[A](), _).getOrElse(TyTop[A]())
}

