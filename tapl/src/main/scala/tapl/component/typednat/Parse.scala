package tapl.component.typednat

import tapl.common._
import tapl.component.nat
import tapl.component.typednat.Type.Factory._

trait Parse[A[-X, Y] <: Term[X, Y], B[-X, Y] <: Type[X, Y]] extends EParser[A] with TParser[B] with nat.Parse[A] {
  lexical.reserved += "Nat"

  lazy val pTypedNatE: Parser[Exp[A]] = pNatE
  lazy val pTypedNatT: Parser[Exp[B]] = "Nat" ^^ { _ => TyNat[B]() }
}
