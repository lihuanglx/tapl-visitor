package tapl.language.simplebool

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.{typed, typedbool}

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends typed.Parse[A, B]
  with typedbool.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  lazy val pSimpleBoolE: Parser[E3[A, Exp[B]]] = pTypedE ||| pTypedBoolE
  lazy val pSimpleBoolT: Parser[Exp[B]] = pTypedT ||| pTypedBoolT

  override lazy val pE: Parser[E3[A, Exp[B]]] = pSimpleBoolE
  override lazy val pT: Parser[Exp[B]] = pSimpleBoolT
}
