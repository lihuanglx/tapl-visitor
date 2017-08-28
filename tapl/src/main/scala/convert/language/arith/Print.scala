package convert.language.arith

import convert.common._
import convert.component.{bool, nat}

trait Print[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], String] with bool.Print[A] with nat.Print[A]
  with Term.ConvertChainNat[A]

object Print extends Print[Term] with Impl[String]
