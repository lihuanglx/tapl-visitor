package convert.component.nat

import convert.common.Exp

trait Print[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], String] with IsNatVal[A] {
  override def tmZero() = "0"

  override def tmSucc(e: Exp[A]): String =
    isNatVal(e) match {
      case Some((x, _)) => (x + 1).toString
      case _ => "succ (" + apply(e) + ")"
    }

  override def tmPred(e: Exp[A]): String = "pred (" + apply(e) + ")"

  override def tmIsZero(e: Exp[A]): String = "iszero (" + apply(e) + ")"
}