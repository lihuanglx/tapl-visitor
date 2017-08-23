package tapl.language.recon

import tapl.common._
import tapl.component.typed
import tapl.language.tyarith

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends tyarith.Parse[A[-?, ?, Exp[B]], B] with typed.Parse[A, B] {

  lazy val pReconE: Parser[Exp2[A, Exp[B]]] = pTypedE ||| pTyArithE
  lazy val pReconT: Parser[Exp[B]] = pTypedT ||| pTyArithT

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pReconE
  override lazy val pT: Parser[Exp[B]] = pReconT
}
