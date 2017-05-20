package tapl.component.typevar

import tapl.common.Exp

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  // todo
  override def TyVar(x: String): (Exp[A]) => Boolean = {
    case CTyVar(y) => x == y
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  // todo
  override def TyVar(x: String): (Exp[A]) => Boolean = {
    case CTyVar(y) => x == y
    case _ => false
  }
}
