package tapl.component.record

import tapl.common.Exp

trait Print[A[-R, _]] extends Alg[Exp[A], String] {
  override def TmRecord(l: List[(String, Exp[A])]): String =
    "{" + l.map(x => x._1 + " = " + apply(x._2)).reduce((x, y) => x + ", " + y) + "}"

  override def TmProj(e: Exp[A], x: String): String = apply(e) + "." + x
}