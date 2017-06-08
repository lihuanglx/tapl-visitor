package tapl.language.bot

import tapl.common._
import tapl.component.{topbot, typed}

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends typed.Parse[A, B] with topbot.Parse[B] {
  lazy val pBotE: Parser[TExp[A, Exp[B]]] = pTypedE
  lazy val pBotT: Parser[Exp[B]] = pTypedT ||| pTopBotT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pBotE
  override lazy val pT: Parser[Exp[B]] = pBotT
}
