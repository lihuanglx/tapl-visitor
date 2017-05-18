package tapl.component.floatstring

import tapl.common.{EParser, Exp}

trait Parse[A[-X, Y] <: Alg[X, Y]] extends EParser[A] {
  lexical.delimiters += "*"

  // todo
  lazy val pFloatStringE: Parser[Exp[A]] =
    chainl1(pE, "*" ^^^ { (e1: Exp[A], e2: Exp[A]) => CTimes(e1, e2) }) |||
      stringLit ^^ CString[A]
}
