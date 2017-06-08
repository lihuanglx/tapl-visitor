package tapl.component.typedbool

import tapl.common.Exp
import tapl.component.bool

trait Print[A[-R, _]] extends Alg[Exp[A], String] with bool.Print[A]

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] {
  override def tyBool(): String = "Bool"
}
