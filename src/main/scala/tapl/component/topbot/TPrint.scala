package tapl.component.topbot

import tapl.common.Exp
import tapl.component.top

trait TPrint[A[-X, Y]] extends TAlg[Exp[A], String] with top.TPrint[A] {
  override def TyBot(): String = "Bot"
}
