package inspect.language.fulluntyped

import inspect.common._
import inspect.component.{floatstring, let, record}
import inspect.language.{arith, untyped}

trait Print[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], String] with arith.Print[A] with untyped.Print[A]
  with floatstring.Print[A] with let.Print[A] with record.Print[A] with Term.AllInspectChains[A]

object Print extends Print[Term] with Impl[String]
