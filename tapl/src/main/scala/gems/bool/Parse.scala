package gems.bool

import gems.common._
import gems.bool.Term.Factory._
import gems.bool.Type.Factory._

trait Parse[A[-X, Y] <: Term[X, Y], B[-X, Y] <: Type[X, Y]] extends BaseParser {
  lexical.reserved += ("true", "false", "if", "then", "else", "Bool")
  lexical.delimiters += ("(", ")")

  private lazy val pTrue = "true" ^^^ TmTrue[A, A]()

  private lazy val pFalse = "false" ^^^ TmFalse[A, A]()

  private lazy val pIf =
    ("if" ~> pE) ~ ("then" ~> pE) ~ ("else" ~> pE) ^^ { case e1 ~ e2 ~ e3 => TmIf[A, A](e1, e2, e3) }

  lazy val pBoolE: PackratParser[Exp[A]] =
    pTrue ||| pFalse ||| pIf ||| "(" ~> pE <~ ")"

  lazy val pBoolT: PackratParser[Exp[B]] =
    "Bool" ^^^ TyBool[B, B]()

  lazy val pE: PackratParser[Exp[A]] = pBoolE

  lazy val pT: PackratParser[Exp[B]] = pBoolT
}

