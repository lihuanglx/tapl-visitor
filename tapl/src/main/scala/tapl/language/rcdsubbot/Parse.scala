package tapl.language.rcdsubbot

import tapl.common._
import tapl.component.typedrecord
import tapl.language.bot

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends typedrecord.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] with bot.Parse[A, B] {

  lazy val pRcdSubBotE: Parser[E3[A, Exp[B]]] = pBotE ||| pTypedRecordE
  lazy val pRcdSubBotT: Parser[Exp[B]] = pBotT ||| pTypedRecordT

  override lazy val pE: Parser[E3[A, Exp[B]]] = pRcdSubBotE
  override lazy val pT: Parser[Exp[B]] = pRcdSubBotT
}
