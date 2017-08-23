package tapl.component.varapp

import tapl.common._
import tapl.component.varapp.Term._

trait Parse[A[-X, Y] <: Term[X, Y]] extends EParser[A] {
  lexical.delimiters += ("(", ")", ";")

  lazy val pVarAppE: Parser[Exp[A]] =
    lcid ^^ TmVar[A] |||
      pE ~ pE ^^ { case e1 ~ e2 => TmApp[A](e1, e2) } |||
      "(" ~> pE <~ ")" |||
      "{" ~> rep1sep(pE, ";") <~ "}" ^^ TmSeq[A]
}
