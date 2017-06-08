package tapl.component.top

import tapl.common._

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyTop(): Exp[A] => Boolean = _ => false
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyTop(): Exp[A] => Boolean = {
    case CTyTop() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def TyTop(): Exp[A] => Exp[A] = _ => CTyTop[A]()
}

trait Meet[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with MeetAux[A] {
  override def TyTop(): Exp[A] => Exp[A] = u => u
}
