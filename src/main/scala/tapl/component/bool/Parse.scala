package tapl.component.bool

import tapl.common.{EParser, Exp}
import tapl.component.bool.Factory._

trait Parse[A[-X, Y] <: Alg[X, Y]] extends EParser[A] {
  lexical.reserved += ("true", "false", "if", "then", "else")
  lexical.delimiters += ("(", ")")

  private lazy val pTrue = "true" ^^ { _ => CTrue[A]() }

  private lazy val pFalse = "false" ^^ { _ => CFalse[A]() }

  private lazy val pIf =
    ("if" ~> pE) ~ ("then" ~> pE) ~ ("else" ~> pE) ^^ { case e1 ~ e2 ~ e3 => CIf[A](e1, e2, e3) }

  private lazy val pParen = "(" ~> pE <~ ")"

  lazy val pBoolE: Parser[Exp[A]] = pTrue ||| pFalse ||| pIf ||| pParen
}
