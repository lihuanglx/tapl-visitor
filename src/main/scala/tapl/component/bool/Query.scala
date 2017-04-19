package tapl.component.bool

import tapl.common.{Default, Exp}

trait Query[A[-R, _], T] extends Alg[Exp[A], T] with Default[T] {
  override def TmTrue(): T = default

  override def TmFalse(): T = default

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): T = default
}
