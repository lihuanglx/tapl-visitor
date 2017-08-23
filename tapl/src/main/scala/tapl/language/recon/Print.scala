package tapl.language.recon

import tapl.common._
import tapl.component.typed
import tapl.language.tyarith

trait Print[A[-R, E, -F], V] extends Term[Exp2[A, V], String, V] with tyarith.Print[A[-?, ?, V]] with typed.Print[A, V]

object Print extends Print[Term, Exp[Type]] with Impl[String] {
  override def printT(t: Exp[Type]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends Type[Exp[A], String]
  with tyarith.TPrint[A] with typed.TPrint[A]

object TPrint extends TPrint[Type] with TImpl[String]
