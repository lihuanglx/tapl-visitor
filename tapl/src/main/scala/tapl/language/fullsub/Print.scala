package tapl.language.fullsub

import tapl.common._
import tapl.component.{typed, extension, top}

trait Print[A[-R, E, -F], V] extends Term[Exp2[A, V], String, V]
  with typed.Print[A, V] with extension.Print[A, V]

object Print extends Print[Term, Exp[Type]] with Impl[String] {
  override def printT(t: Exp[Type]): String = t(TPrint)
}

trait TPrint[A[-F, T]] extends Type[Exp[A], String]
  with typed.TPrint[A] with extension.TPrint[A] with top.TPrint[A]

object TPrint extends TPrint[Type] with TImpl[String]
