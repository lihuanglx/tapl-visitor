package inspect.language.arith

import inspect.common._
import inspect.component.{bool, nat}

trait Print[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], String] with bool.Print[A] with nat.Print[A]
  with Term.InspectChainNat[A]

object Print extends Print[Term] with Impl[String]
