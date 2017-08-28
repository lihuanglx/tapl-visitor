package convert.language.arith

import convert.common._
import convert.component.{bool, nat}

trait Parse[A[-X, Y] <: Term[X, Y]] extends bool.Parse[A] with nat.Parse[A] {
  lazy val pArithE: Parser[Exp[A]] = pBoolE ||| pNatE

  override lazy val pE: Parser[Exp[A]] = pArithE
}
