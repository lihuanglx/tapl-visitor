package tapl.component.rectype

import tapl.common.Exp

trait TPrint[A[-X, Y]] extends Type[Exp[A], String] {
  override def tyRec(x: String, t: Exp[A]): String = "(Rec " + x + "." + apply(t) + ")"
}
