package tapl.component.varapp

import tapl.common.{EParser, Exp}

trait Parse[A[-X, Y] <: Alg[X, Y]] extends EParser[A] {
  lexical.delimiters += ("(", ")")

  val f: Factory[A]

  val pVarAppE: Parser[Exp[A]] =
    lcid ^^ f.TmVar |||
      pE ~ pE ^^ { case e1 ~ e2 => f.TmApp(e1, e2) } |||
      "(" ~> pE <~ ")"
}
