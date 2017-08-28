package convert.component.varapp

import convert.common._
import convert.component.varapp.Term._

trait Parse[A[-X, Y] <: Term[X, Y]] extends EParser[A] {
  lexical.delimiters += ("(", ")", ";")

  lazy val pVarAppE: Parser[Exp[A]] =
    lcid ^^ TmVar[A, A] |||
      pE ~ pE ^^ { case e1 ~ e2 => TmApp[A, A](e1, e2) } |||
      "(" ~> pE <~ ")" |||
      "{" ~> rep1sep(pE, ";") <~ "}" ^^ TmSeq[A, A]
}
