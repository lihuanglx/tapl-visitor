package tapl.component.varapp

import tapl.common.{CommonParser, Exp}

trait Parse[A[-X, Y] <: Alg[X, Y]] extends CommonParser[Exp[A]] {
  lexical.delimiters += ("(", ")")

  val f: Factory[A]

  private lazy val pApp = pE ~ pE ^^ { case e1 ~ e2 => f.TmApp(e1, e2) }

  private lazy val pVar = lcid ^^ f.TmVar

  private lazy val pVarAppE: Parser[Exp[A]] = pVar ||| pApp ||| "(" ~> pE <~ ")"

  val pE: Parser[Exp[A]]
}
