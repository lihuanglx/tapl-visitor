package tapl.component.lambda

import tapl.common.{EParser, Exp}

trait Parse[A[-X, Y] <: Alg[X, Y]] extends EParser[A] {
  lexical.delimiters += ("\\", ".")

  val f: Factory[A]

  val pLamE: Parser[Exp[A]] =
    ("\\" ~> lcid) ~ ("." ~> pE) ^^ { case x ~ e0 => f.TmAbs(x, e0) }
}
