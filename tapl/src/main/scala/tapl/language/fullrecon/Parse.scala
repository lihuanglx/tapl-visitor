package tapl.language.fullrecon

import tapl.common._
import tapl.component.let
import tapl.language.recon

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends recon.Parse[A, B] with let.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam] {

  lazy val pFullReconE: Parser[E3[A, Exp[B]]] = pReconE ||| pLetE
  lazy val pFullReconT: Parser[Exp[B]] = pReconT

  override lazy val pE: Parser[E3[A, Exp[B]]] = pFullReconE
  override lazy val pT: Parser[Exp[B]] = pFullReconT
}
