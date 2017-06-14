package tapl.component.unit

import tapl.common._

trait Print[A[-R, E]] extends Alg[Exp[A], String] {
  override def tmUnit(): String = "unit"
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] {
  override def tyUnit(): String = "Unit"
}
