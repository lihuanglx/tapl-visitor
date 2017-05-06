package tapl.component.lambda

import tapl.common.{EParser, Exp}
import tapl.component.lambda.Factory._

trait Parse[A[-X, Y] <: Alg[X, Y]] extends EParser[A] {
  lexical.delimiters += ("\\", ".")

  val pLamE: Parser[Exp[A]] =
    ("\\" ~> lcid) ~ ("." ~> pE) ^^ { case x ~ e0 => CAbs(x, e0) }
}
