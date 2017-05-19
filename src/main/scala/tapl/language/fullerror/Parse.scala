package tapl.language.fullerror

import tapl.common.Exp
import tapl.common.Util._
import tapl.component.{topbot, typed2, typedbool}

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends topbot.Parse[B]
  with typed2.Parse[A, B] with typedbool.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  lexical.reserved += ("error", "try", "with")

  private val pErrorE: Parser[E3[A, Exp[B]]] =
    "error" ^^ { _ => CError[A, Exp[B]]() } |||
      "try" ~> pE ~ ("with" ~> pE) ^^ { case e1 ~ e2 => CTry[A, Exp[B]](e1, e2) }

  lazy val pFullErrorE: Parser[E3[A, Exp[B]]] = pTyped2E ||| pTypedBoolE ||| pErrorE
  lazy val pFullErrorT: Parser[Exp[B]] = pTopBotT ||| pTyped2T ||| pTypedBoolT

  override lazy val pE: Parser[E3[A, Exp[B]]] = pFullErrorE
  override lazy val pT: Parser[Exp[B]] = pFullErrorT
}
