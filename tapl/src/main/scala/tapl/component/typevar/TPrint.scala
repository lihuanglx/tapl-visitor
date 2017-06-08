package tapl.component.typevar

import tapl.common.Exp

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] {
  override def tyVar(x: String): String = x
}
