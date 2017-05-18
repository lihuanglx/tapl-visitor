package tapl.component.top

import tapl.common.Exp

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyTop(): (Exp[A]) => Boolean = _ => false
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyTop(): (Exp[A]) => Boolean = {
    case CTyTop() => true
    case _ => false
  }
}
