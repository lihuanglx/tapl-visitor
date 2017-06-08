package tapl.language.tyarith

import tapl.common._
import tapl.component.{typedbool, typednat}

trait Parse[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends typedbool.Parse[A, B] with typednat.Parse[A, B] {
  lazy val pTyArithE: Parser[Exp[A]] = pTypedBoolE ||| pTypedNatE
  lazy val pTyArithT: Parser[Exp[B]] = pTypedBoolT ||| pTypedNatT

  override lazy val pE: Parser[Exp[A]] = pTyArithE
  override lazy val pT: Parser[Exp[B]] = pTyArithT
}
