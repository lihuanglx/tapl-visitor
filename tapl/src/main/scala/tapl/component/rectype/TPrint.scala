package tapl.component.rectype

import tapl.common.Exp
import tapl.component.typevar

trait TPrint[A[-X, Y]] extends TAlg[Exp[A], String] with typevar.TPrint[A] {
  override def tyRec(x: String, t: Exp[A]): String = "(Rec " + x + "." + apply(t) + ")"
}
