package tapl.language.tyarith

import tapl.common._
import tapl.component.{typedbool, typednat}

trait Print[A[-R, _]] extends Term[Exp[A], String] with typedbool.Print[A] with typednat.Print[A]

object Print extends Print[Term] with Impl[String]

trait TPrint[A[-R, _]] extends Type[Exp[A], String] with typedbool.TPrint[A] with typednat.TPrint[A]

object TPrint extends TPrint[Type] with TImpl[String]
