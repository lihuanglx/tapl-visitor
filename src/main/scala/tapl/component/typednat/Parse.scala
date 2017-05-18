package tapl.component.typednat

import tapl.common.{EParser, Exp, TParser}
import tapl.component.nat

trait Parse[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends EParser[A] with TParser[B] with nat.Parse[A] {
  lexical.reserved += "Nat"

  lazy val pTypedNatE: Parser[Exp[A]] = pNatE
  lazy val pTypedNatT: Parser[Exp[B]] = "Nat" ^^ { _ => CTyNat[B]() }
}
