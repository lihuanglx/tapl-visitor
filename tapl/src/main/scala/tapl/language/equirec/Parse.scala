package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed}

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends typed.Parse[A, B] with rectype.Parse[B] {

  lazy val pEquiRecE: Parser[Exp2[A, Exp[B]]] = pTypedE
  lazy val pEquiRecT: Parser[Exp[B]] = pTypedT ||| pRecTypeT

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pEquiRecE
  override lazy val pT: Parser[Exp[B]] = pEquiRecT
}
