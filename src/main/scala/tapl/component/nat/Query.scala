package tapl.component.nat

import tapl.common.{Default, Exp}

trait Query[A[-R, _], T] extends Alg[Exp[A], T] with Default[T] {
  override def TmZero(): T = default

  override def TmPred(e: Exp[A]): T = default

  override def TmSucc(e: Exp[A]): T = default

  override def TmIsZero(e: Exp[A]): T = default
}
