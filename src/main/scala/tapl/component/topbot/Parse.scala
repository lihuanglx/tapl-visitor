package tapl.component.topbot

import tapl.common.Exp
import tapl.component.top
import tapl.component.topbot.TFactory._

trait Parse[A[-X, Y] <: TAlg[X, Y]] extends top.Parse[A] {
  lexical.reserved += "Bot"

  lazy val pTopBotT: Parser[Exp[A]] = pTopT ||| "Bot" ^^ { _ => CTyBot[A]() }
}