package tapl.component.bottom

import tapl.common._
import tapl.component.bottom.TAlg.Factory._

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyBot(): (Exp[A]) => Boolean = _ => true
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyBot(): (Exp[A]) => Boolean = {
    case TyBot() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] {
  override def tyBot(): Exp[A] => Exp[A] = u => u
}

trait Meet[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] {
  override def tyBot(): Exp[A] => Exp[A] = _ => TyBot[A]()
}
