package tapl.language.rcdsubbot

import tapl.common._
import tapl.component.typedrecord
import tapl.language.bot

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends typedrecord.Parse[A[-?, ?, Exp[B]], B] with bot.Parse[A, B] {

  lazy val pRcdSubBotE: Parser[Exp2[A, Exp[B]]] = pBotE ||| pTypedRecordE
  lazy val pRcdSubBotT: Parser[Exp[B]] = pBotT ||| pTypedRecordT

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pRcdSubBotE
  override lazy val pT: Parser[Exp[B]] = pRcdSubBotT
}
