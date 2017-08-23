package tapl.component.top

import tapl.common.Exp

trait TPrint[A[-X, Y]] extends Type[Exp[A], String] {
  override def tyTop(): String = "Top"
}
