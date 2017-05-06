package tapl.language.tyarith

import tapl.common.Exp
import tapl.component.{typedbool, typednat}

trait Parse[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends typedbool.Parse[A, B] with typednat.Parse[A, B] {
  val pTyArithE: Parser[Exp[A]] = pTypedBoolE ||| pTypedNatE
  val pTyArithT: Parser[Exp[B]] = pTypedBoolT ||| pTypedNatT

  override val pE: Parser[Exp[A]] = pTyArithE
  override val pT: Parser[Exp[B]] = pTyArithT

  def parse(inp: String): Option[Exp[A]] = parseBy(pE)(inp)
}
