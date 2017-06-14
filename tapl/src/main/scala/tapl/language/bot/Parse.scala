package tapl.language.bot

import tapl.common._
import tapl.component.{top, bottom, typed}

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends typed.Parse[A, B] with top.Parse[B] with bottom.Parse[B] {

  lazy val pBotE: Parser[TExp[A, Exp[B]]] = pTypedE
  lazy val pBotT: Parser[Exp[B]] = pTypedT ||| pTopT ||| pBottomT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pBotE
  override lazy val pT: Parser[Exp[B]] = pBotT
}
