package tapl.component.unit

import tapl.common._

trait Print[A[-R, E]] extends Term[Exp[A], String] {
  override def tmUnit(): String = "unit"
}

trait TPrint[A[-R, _]] extends Type[Exp[A], String] {
  override def tyUnit(): String = "Unit"
}
