package tapl.language.tyarith

import tapl.common.Exp
import tapl.component.{typedbool, typednat}

trait Print[A[-R, _]] extends Alg[Exp[A], String] with typedbool.Print[A] with typednat.Print[A]

object PrintImpl extends Print[Alg] with Impl[String]
