package tapl.component.rectype

import tapl.common.Exp

trait TPrint[A[-X, Y]] extends TAlg[Exp[A], String] {
  override def TyRec(x: String, t: Exp[A]): String = "Rec " + x + "." + apply(t)
}
