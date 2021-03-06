package tapl.component.bool

import tapl.common._
import tapl.component.bool.Term._

trait Parse[A[-X, Y] <: Term[X, Y]] extends EParser[A] {
  lexical.reserved += ("true", "false", "if", "then", "else")
  lexical.delimiters += ("(", ")")

  private lazy val pTrue = "true" ^^ { _ => TmTrue[A]() }

  private lazy val pFalse = "false" ^^ { _ => TmFalse[A]() }

  private lazy val pIf =
    ("if" ~> pE) ~ ("then" ~> pE) ~ ("else" ~> pE) ^^ { case e1 ~ e2 ~ e3 => TmIf[A](e1, e2, e3) }

  private lazy val pParen = "(" ~> pE <~ ")"

  lazy val pBoolE: Parser[Exp[A]] = pTrue ||| pFalse ||| pIf ||| pParen
}
