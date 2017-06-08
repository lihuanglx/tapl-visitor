package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed, typevar}

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends typed.Parse[A, B]
  with rectype.Parse[B] with typevar.Parse[B] {

  lazy val pEquiRecE: Parser[E3[A, Exp[B]]] = pTypedE
  lazy val pEquiRecT: Parser[Exp[B]] = pTypedT ||| pRecTypeT ||| pTypeVarT

  override lazy val pE: Parser[E3[A, Exp[B]]] = pEquiRecE
  override lazy val pT: Parser[Exp[B]] = pEquiRecT
}
