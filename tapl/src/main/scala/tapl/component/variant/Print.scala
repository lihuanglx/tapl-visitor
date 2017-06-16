package tapl.component.variant

import tapl.common._

trait Print[A[-R, E, -F], V] extends Alg[Exp2[A, V], String, V] with PrintT[V] {
  override def tmTag(x: String, e: Exp2[A, V], t: V): String = "<" + x + "=" + apply(e) + "> as " + printT(t)

  override def tmCase(e: Exp2[A, V], l: List[(String, String, Exp2[A, V])]): String =
    "case " + apply(e) + " of " +
      l.map(x => "<" + x._1 + "=" + x._2 + "> => " + apply(x._3)).reduce((x, y) => x + " | " + y)
}

trait TPrint[A[-F, T]] extends TAlg[Exp[A], String] {
  override def tyVariant(l: List[(String, Exp[A])]): String =
    "<" + l.map(x => x._1 + ":" + apply(x._2)).reduce((x, y) => x + ", " + y) + ">"
}
