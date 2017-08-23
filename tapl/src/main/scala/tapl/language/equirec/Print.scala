package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed}

trait Print[A[-R, E, -F], V] extends Term[Exp2[A, V], String, V] with typed.Print[A, V]

object Print extends Print[Term, Exp[Type]] with Impl[String] {
  override def printT(t: Exp[Type]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends Type[Exp[A], String]
  with typed.TPrint[A] with rectype.TPrint[A]

object TPrint extends TPrint[Type] with TImpl[String]
