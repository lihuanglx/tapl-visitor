package tapl.language.fullsub

import tapl.common._
import tapl.component.{simple, top}

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends simple.Parse[A, B] with top.Parse[B] {

  lazy val pFullSubE: Parser[TExp[A, Exp[B]]] = pSimpleE
  lazy val pFullSubT: Parser[Exp[B]] = pSimpleT ||| pTopT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pFullSubE
  override lazy val pT: Parser[Exp[B]] = pFullSubT
}
