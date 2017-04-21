package tapl.component.lambda

import tapl.common.{Default, Exp}

trait Query[A[-R, _], T] extends Alg[Exp[A], T] with Default[T] {
  override def TmAbs(x: String, e: Exp[A]): T = default
}
