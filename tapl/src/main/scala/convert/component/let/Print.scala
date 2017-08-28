package convert.component.let

import convert.common.Exp

trait Print[A[-R, _]] extends Term[Exp[A], String] {
  override def tmLet(x: String, e1: Exp[A], e2: Exp[A]): String =
    "let " + x + " = " + apply(e1) + " in " + apply(e2)
}
