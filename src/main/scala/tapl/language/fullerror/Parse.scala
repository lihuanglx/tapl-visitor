package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends bot.Parse[A, B]
  with typedbool.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  lexical.reserved += ("error", "try", "with")

  private val pErrorE: Parser[E3[A, Exp[B]]] =
    "error" ^^ { _ => CError[A, Exp[B]]() } |||
      "try" ~> pE ~ ("with" ~> pE) ^^ { case e1 ~ e2 => CTry[A, Exp[B]](e1, e2) }

  lazy val pFullErrorE: Parser[E3[A, Exp[B]]] = pBotE ||| pTypedBoolE ||| pErrorE
  lazy val pFullErrorT: Parser[Exp[B]] = pBotT ||| pTypedBoolT

  override lazy val pE: Parser[E3[A, Exp[B]]] = pFullErrorE
  override lazy val pT: Parser[Exp[B]] = pFullErrorT
}
