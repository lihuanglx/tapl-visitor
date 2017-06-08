package tapl.component.topbot

import tapl.common._
import tapl.component.top
import tapl.component.topbot.TAlg.Factory._

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with top.SubtypeOf[A] {
  override def tyBot(): (Exp[A]) => Boolean = _ => true
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with top.TEquals[A] {
  override def tyBot(): (Exp[A]) => Boolean = {
    case TyBot() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with top.Join[A] {
  override def tyBot(): Exp[A] => Exp[A] = u => u
}

trait Meet[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with top.Meet[A] {
  override def tyBot(): Exp[A] => Exp[A] = _ => TyBot[A]()
}
