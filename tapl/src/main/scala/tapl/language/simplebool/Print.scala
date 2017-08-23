package tapl.language.simplebool

import tapl.common._
import tapl.component.{typed, typedbool}

trait Print[A[-R, E, -F], V] extends Term[Exp2[A, V], String, V] with typed.Print[A, V] with typedbool.Print[A[-?, ?, V]]

trait TPrint[A[-R, _]] extends Type[Exp[A], String] with typed.TPrint[A] with typedbool.TPrint[A]

object Print extends Print[Term, Exp[Type]] with Impl[String] {
  override def printT(t: Exp[Type]): String = t(TPrint)
}

object TPrint extends TPrint[Type] {
  override def apply(t: Exp[Type]): String = t(this)
}
