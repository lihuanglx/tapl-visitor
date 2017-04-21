package tapl.component.bool

import tapl.common.{CommonParser, Exp}

trait Parse[A[-X, Y] <: Alg[X, Y]] extends CommonParser[Exp[A]] {
  lexical.reserved += ("true", "false", "if", "then", "else")
  lexical.delimiters += ("(", ")")

  val f: Factory[A]

  private lazy val pTrue = "true" ^^ { _ => f.TmTrue() }

  private lazy val pFalse = "false" ^^ { _ => f.TmFalse() }

  private lazy val pIf = ("if" ~> pE) ~ ("then" ~> pE) ~ ("else" ~> pE) ^^ { case e1 ~ e2 ~ e3 => f.TmIf(e1, e2, e3) }

  private lazy val pParen: Parser[Exp[A]] = "(" ~> pE <~ ")"

  lazy val pBoolE: Parser[Exp[A]] = pTrue ||| pFalse ||| pIf ||| pParen

  val pE: Parser[Exp[A]]
}
