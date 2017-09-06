package convert.language.fulluntyped

import convert.common._
import convert.component.{floatstring, let, record}
import convert.language.{arith, untyped}

trait Print[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], String] with arith.Print[A] with untyped.Print[A]
  with floatstring.Print[A] with let.Print[A] with record.Print[A] with Term.AllChains[A]

object Print extends Print[Term] with Impl[String]
