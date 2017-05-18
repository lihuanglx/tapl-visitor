package tapl.component.typed2

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.typed

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V] with typed.Print[A, V]

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] with typed.TPrint[A] {
  override def TyVar(x: String): String = x
}
