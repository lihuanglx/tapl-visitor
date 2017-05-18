package tapl.component.variant

import tapl.common.{Exp, PrintT}
import tapl.common.Util.E3

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V] with PrintT[V] {
  override def TmTag(x: String, e: E3[A, V], t: V): String = "<" + x + "=" + apply(e) + "> as " + printT(t)

  override def TmCase(e: E3[A, V], l: List[(String, String, E3[A, V])]): String =
    "case " + apply(e) + " of " +
      l.map(x => "<" + x._1 + "=" + x._2 + "> => " + apply(x._3)).reduce((x, y) => x + " | " + y)
}

trait TPrint[A[-F, T]] extends TAlg[Exp[A], String] {
  override def TyVariant(l: List[(String, Exp[A])]): String =
    "<" + l.map(x => x._1 + ":" + apply(x._2)).reduce((x, y) => x + ", " + y) + ">"
}
