package tapl.component.top

import tapl.common.Exp

trait TPrint[A[-X, Y]] extends TAlg[Exp[A], String] {
  override def tyTop(): String = "Top"
}
