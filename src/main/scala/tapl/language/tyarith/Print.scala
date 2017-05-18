package tapl.language.tyarith

import tapl.common.Exp
import tapl.component.{typedbool, typednat}

trait Print[A[-R, _]] extends Alg[Exp[A], String] with typedbool.Print[A] with typednat.Print[A]

object Print extends Print[Alg] with Impl[String]

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] with typedbool.TPrint[A] with typednat.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]
