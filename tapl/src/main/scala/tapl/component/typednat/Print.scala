package tapl.component.typednat

import tapl.common.Exp
import tapl.component.nat

trait Print[A[-R, _]] extends Term[Exp[A], String] with nat.Print[A]

trait TPrint[A[-R, _]] extends Type[Exp[A], String] {
  override def tyNat(): String = "Nat"
}
