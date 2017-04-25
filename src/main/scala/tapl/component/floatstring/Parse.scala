package tapl.component.floatstring

import tapl.common.{EParser, Exp}

trait Parse[A[-X, Y] <: Alg[X, Y]] extends EParser[A] {
  lexical.delimiters += "*"

  val f: Factory[A]

  // todo
  val pFloatStringE: Parser[Exp[A]] =
    chainl1(pE, "*" ^^^ { (e1: Exp[A], e2: Exp[A]) => f.TmTimes(e1, e2) }) |||
      stringLit ^^ f.TmString
}
