package tapl.component.bottom

import tapl.common._

trait TPrint[A[-X, Y]] extends Type[Exp[A], String] {
  override def tyBot(): String = "Bot"
}
