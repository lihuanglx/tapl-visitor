package tapl.component.typevar

import tapl.common.Exp
import tapl.component.top
import tapl.component.top.CTyTop

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

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] {
  override def TyVar(x: String): Exp[A] => Exp[A] = {
    case CTyVar(y) if x == y => CTyVar[A](x)
    case _ => CTyTop[A]()
  }
}
