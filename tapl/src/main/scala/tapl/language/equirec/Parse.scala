package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed}

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends typed.Parse[A, B] with rectype.Parse[B] {

  lazy val pEquiRecE: Parser[TExp[A, Exp[B]]] = pTypedE
  lazy val pEquiRecT: Parser[Exp[B]] = pTypedT ||| pRecTypeT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pEquiRecE
  override lazy val pT: Parser[Exp[B]] = pEquiRecT
}
