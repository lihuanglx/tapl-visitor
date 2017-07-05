package tapl.component.top

import tapl.common._
import tapl.component.top.TAlg.Factory._

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyTop(): Exp[A] => Boolean = {
    case TyTop() => true
    case _ => false
  }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyTop(): Exp[A] => Boolean = {
    case TyTop() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def tyTop(): Exp[A] => Exp[A] = _ => TyTop[A]()
}

trait Meet[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with MeetAux[A] {
  override def tyTop(): Exp[A] => Exp[A] = u => u
}
