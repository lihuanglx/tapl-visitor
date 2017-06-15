package tapl.language.fullrecon

import tapl.common._
import tapl.component.let
import tapl.language.recon
import tapl.language.fullrecon.Alg.Factory._

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends recon.Parse[A, B] with let.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam] {

  lazy val pFullReconE: Parser[TExp[A, Exp[B]]] = pReconE ||| pLetE |||
    ("\\" ~> lcid) ~ ("." ~> pE) ^^ { case x ~ e0 => TmUAbs(x, e0) }
  lazy val pFullReconT: Parser[Exp[B]] = pReconT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pFullReconE
  override lazy val pT: Parser[Exp[B]] = pFullReconT
}
