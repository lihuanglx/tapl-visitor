package inspect.language.arith

import inspect.common._
import inspect.component.{bool, nat}

trait Parse[A[-X, Y] <: Term[X, Y]] extends bool.Parse[A] with nat.Parse[A] {
  lazy val pArithE: Parser[Exp[A]] = pBoolE ||| pNatE

  override lazy val pE: Parser[Exp[A]] = pArithE
}
