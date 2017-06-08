package tapl.component.let

import tapl.common.Exp

trait Print[A[-R, _]] extends Alg[Exp[A], String] {
  override def tmLet(x: String, e1: Exp[A], e2: Exp[A]): String =
    "let " + x + " = " + apply(e1) + " in " + apply(e2)
}
