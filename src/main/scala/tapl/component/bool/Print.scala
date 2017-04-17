package tapl.component.bool

import tapl.common.Exp

trait Print[A[-R, _]] extends Alg[Exp[A], String] {
  override def TmTrue() = "true"

  override def TmFalse() = "false"

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): String =
    "if (" + visit(e1) + ") then (" + visit(e2) + ") else (" + visit(e3) + ")"
}