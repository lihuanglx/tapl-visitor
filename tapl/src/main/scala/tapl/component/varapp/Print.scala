package tapl.component.varapp

import tapl.common.Exp

trait Print[A[-R, _]] extends Term[Exp[A], String] {
  override def tmVar(x: String): String = x

  override def tmApp(e1: Exp[A], e2: Exp[A]): String = "(" + apply(e1) + " " + apply(e2) + ")"

  override def tmSeq(es: List[Exp[A]]): String =
    "{" + es.map(apply).reduceLeft(_ ++ "; " ++ _) + "}"
}
