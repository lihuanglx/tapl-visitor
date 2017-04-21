package tapl.component.varapp

import tapl.common.{Default, Exp}

trait Query[A[-R, _], T] extends Alg[Exp[A], T] with Default[T] {
  override def TmVar(x: String): T = default

  override def TmApp(e1: Exp[A], e2: Exp[A]): T = default
}
