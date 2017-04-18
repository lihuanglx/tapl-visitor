package tapl.component.bool

import tapl.common.Exp

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

trait Parse[A[-X, Y] <: Alg[X, Y]] extends StandardTokenParsers with PackratParsers {
  // todo: fix
  //lexical.reserved += ("true", "false", "if", "then", "else")
  //lexical.delimiters += ("(", ")")

  val f: Factory[A]

  lazy val pBoolE: Parser[Exp[A]] =
    // todo: check
    "true" ^^^ f.TmTrue |||
      "false" ^^ { _ => f.TmFalse() } |||
      ("if" ~> pE) ~ ("then" ~> pE) ~ ("else" ~> pE) ^^ { case e1 ~ e2 ~ e3 => f.TmIf(e1, e2, e3) } |||
      "(" ~> pE <~ ")"

  val pE: Parser[Exp[A]]
}