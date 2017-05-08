package tapl.component.varapp

import tapl.common.{EParser, Exp}
import tapl.component.varapp.Factory._

trait Parse[A[-X, Y] <: Alg[X, Y]] extends EParser[A] {
  lexical.delimiters += ("(", ")")

  lazy val pVarAppE: Parser[Exp[A]] =
    lcid ^^ CVar[A] |||
      pE ~ pE ^^ { case e1 ~ e2 => CApp[A](e1, e2) } |||
      "(" ~> pE <~ ")"
}
