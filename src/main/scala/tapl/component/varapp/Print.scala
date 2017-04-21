package tapl.component.varapp

import tapl.common.Exp

trait Print[A[-R, _]] extends Alg[Exp[A], String] {
  override def TmVar(x: String): String = x

  override def TmApp(e1: Exp[A], e2: Exp[A]): String = "(" + apply(e1) + " " + apply(e2) + ")"
}
