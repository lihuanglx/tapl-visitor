package tapl.component.typedrecord

import tapl.common.Exp
import tapl.component.record

trait Print[A[-R, _]] extends Alg[Exp[A], String] with record.Print[A]

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] {
  override def tyRecord(l: List[(String, Exp[A])]): String =
    "{" + l.map(x => x._1 + ": " + apply(x._2)).reduce((x, y) => x + ", " + y) + "}"
}
