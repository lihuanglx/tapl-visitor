package tapl.language.recon

import tapl.common._
import tapl.component.typed
import tapl.language.tyarith

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends tyarith.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] with typed.Parse[A, B] {

  lazy val pReconE: Parser[TExp[A, Exp[B]]] = pTypedE ||| pTyArithE
  lazy val pReconT: Parser[Exp[B]] = pTypedT ||| pTyArithT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pReconE
  override lazy val pT: Parser[Exp[B]] = pReconT
}
