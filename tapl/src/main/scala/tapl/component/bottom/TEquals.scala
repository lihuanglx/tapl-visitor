package tapl.component.bottom

import tapl.common._
import tapl.component.bottom.Type.Factory._

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyBot(): (Exp[A]) => Boolean = _ => true
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyBot(): (Exp[A]) => Boolean = {
    case TyBot() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]] {
  override def tyBot(): Exp[A] => Exp[A] = u => u
}

trait Meet[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]] {
  override def tyBot(): Exp[A] => Exp[A] = _ => TyBot[A]()
}
