package tapl.component.unit

import tapl.common._
import tapl.component.top
import tapl.component.top.Type.Factory._
import tapl.component.unit.Type.Factory._

trait Typer[A[-R, E] <: Term[R, E], B[-X, Y] <: Type[X, Y]] extends Term[Exp[A], Exp[B]] {
  override def tmUnit(): Exp[B] = TyUnit[B]()
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyUnit(): (Exp[A]) => Boolean = {
    case TyUnit() => true
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyUnit(): Exp[A] => Boolean = {
    case TyTop() => true
    case TyUnit() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: Type[X, Y] with top.Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def tyUnit(): Exp[A] => Exp[A] = directJoin(TyUnit[A](), _).getOrElse(TyTop[A]())
}

