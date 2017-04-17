package tapl.component.nat

import tapl.common.Exp

trait Print[A[-R, _]] extends Alg[Exp[A], String] {
  override def TmZero() = "0"

  override def TmSucc(e: Exp[A]): String = "succ (" + visit(e) + ")"

  override def TmPred(e: Exp[A]): String = "pred (" + visit(e) + ")"

  override def TmIsZero(e: Exp[A]): String = "iszero (" + visit(e) + ")"
}