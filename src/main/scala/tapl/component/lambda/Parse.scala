package tapl.component.lambda

import tapl.common.{CommonParser, Exp}

trait Parse[A[-X, Y] <: Alg[X, Y]] extends CommonParser[Exp[A]] {
  lexical.delimiters += ("\\", ".")

  val f: Factory[A]

  lazy val pLamE: Parser[Exp[A]] =
    ("\\" ~> lcid) ~ ("." ~> pE) ^^ { case x ~ e0 => f.TmAbs(x, e0) }

  val pE: Parser[Exp[A]]
}
