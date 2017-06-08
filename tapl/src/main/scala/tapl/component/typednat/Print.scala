package tapl.component.typednat

import tapl.common.Exp
import tapl.component.nat

trait Print[A[-R, _]] extends Alg[Exp[A], String] with nat.Print[A]

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] {
  override def TyNat(): String = "Nat"
}
