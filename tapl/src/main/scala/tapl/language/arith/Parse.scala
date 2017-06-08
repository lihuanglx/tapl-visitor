package tapl.language.arith

import tapl.common._
import tapl.component.{bool, nat}

trait Parse[A[-X, Y] <: Alg[X, Y]] extends bool.Parse[A] with nat.Parse[A] {
  lazy val pArithE: Parser[Exp[A]] = pBoolE ||| pNatE

  override lazy val pE: Parser[Exp[A]] = pArithE
}