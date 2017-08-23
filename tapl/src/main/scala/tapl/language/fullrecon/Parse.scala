package tapl.language.fullrecon

import tapl.common._
import tapl.component.let
import tapl.language.recon
import tapl.language.fullrecon.Term.Factory._

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends recon.Parse[A, B] with let.Parse[A[-?, ?, Exp[B]]] {

  lazy val pFullReconE: Parser[Exp2[A, Exp[B]]] = pReconE ||| pLetE |||
    ("\\" ~> lcid) ~ ("." ~> pE) ^^ { case x ~ e0 => TmUAbs(x, e0) }
  lazy val pFullReconT: Parser[Exp[B]] = pReconT

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pFullReconE
  override lazy val pT: Parser[Exp[B]] = pFullReconT
}
