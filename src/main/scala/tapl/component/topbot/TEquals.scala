package tapl.component.topbot

import tapl.common.Exp
import tapl.component.top

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with top.SubtypeOf[A] {
  override def TyBot(): (Exp[A]) => Boolean = _ => true
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with top.TEquals[A] {
  override def TyBot(): (Exp[A]) => Boolean = {
    case CTyBot() => true
    case _ => false
  }
}
