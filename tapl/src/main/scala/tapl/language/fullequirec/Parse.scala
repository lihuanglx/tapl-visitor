package tapl.language.fullequirec

import tapl.common._
import tapl.component.{rectype, typevar}
import tapl.language.fullsimple

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends fullsimple.Parse[A, B] with rectype.Parse[B] with typevar.Parse[B] {

  lazy val pFullEquiRecE: Parser[TExp[A, Exp[B]]] = pFullSimpleE
  lazy val pFullEquiRecT: Parser[Exp[B]] = pFullSimpleT ||| pRecTypeT ||| pTypeVarT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pFullEquiRecE
  override lazy val pT: Parser[Exp[B]] = pFullEquiRecT
}
