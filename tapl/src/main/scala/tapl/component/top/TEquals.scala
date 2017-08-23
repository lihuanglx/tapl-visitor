package tapl.component.top

import tapl.common._
import tapl.component.top.Type.Factory._

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyTop(): Exp[A] => Boolean = {
    case TyTop() => true
    case _ => false
  }
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyTop(): Exp[A] => Boolean = {
    case TyTop() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def tyTop(): Exp[A] => Exp[A] = _ => TyTop[A]()
}

trait Meet[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]] with MeetAux[A] {
  override def tyTop(): Exp[A] => Exp[A] = u => u
}
