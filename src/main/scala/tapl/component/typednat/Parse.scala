package tapl.component.typednat

import tapl.common.{ETParser, Exp}
import tapl.component.nat
import tapl.component.typednat.TFactory._

trait Parse[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends ETParser[A, B] with nat.Parse[A] {
  lexical.reserved += "Nat"

  lazy val pTypedNatE: Parser[Exp[A]] = pNatE
  lazy val pTypedNatT: Parser[Exp[B]] = "Nat" ^^ { _ => CTyNat[B]() }
}
