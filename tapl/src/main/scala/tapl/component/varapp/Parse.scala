package tapl.component.varapp

import tapl.common._
import tapl.component.varapp.Alg._

trait Parse[A[-X, Y] <: Alg[X, Y]] extends EParser[A] {
  lexical.delimiters += ("(", ")")

  lazy val pVarAppE: Parser[Exp[A]] =
    lcid ^^ TmVar[A] |||
      pE ~ pE ^^ { case e1 ~ e2 => TmApp[A](e1, e2) } |||
      "(" ~> pE <~ ")"
}
