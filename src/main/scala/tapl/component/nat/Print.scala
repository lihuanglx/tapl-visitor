package tapl.component.nat

import tapl.common.Exp


trait Print[A[-R, _]] extends Alg[Exp[A], String] {
  val isNumVal: A[Exp[A], Option[Int]]

  override def TmZero() = "0"

  override def TmSucc(e: Exp[A]): String =
    e(isNumVal) match {
      case Some(x) => (x + 1).toString
      case _ => "succ (" + apply(e) + ")"
    }

  override def TmPred(e: Exp[A]): String =
    e(isNumVal) match {
      case Some(x) => (x - 1).toString
      case _ => "pred (" + apply(e) + ")"
    }

  override def TmIsZero(e: Exp[A]): String = "iszero (" + apply(e) + ")"
}