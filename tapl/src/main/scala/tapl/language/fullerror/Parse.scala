package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot
import tapl.language.fullerror.Alg.Factory._

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends bot.Parse[A, B]
  with typedbool.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  lexical.reserved += ("error", "try", "with")

  private val pErrorE: Parser[Exp2[A, Exp[B]]] =
    "error" ^^ { _ => TmError[A, Exp[B]]() } |||
      "try" ~> pE ~ ("with" ~> pE) ^^ { case e1 ~ e2 => TmTry[A, Exp[B]](e1, e2) }

  lazy val pFullErrorE: Parser[Exp2[A, Exp[B]]] = pBotE ||| pTypedBoolE ||| pErrorE
  lazy val pFullErrorT: Parser[Exp[B]] = pBotT ||| pTypedBoolT

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pFullErrorE
  override lazy val pT: Parser[Exp[B]] = pFullErrorT
}
