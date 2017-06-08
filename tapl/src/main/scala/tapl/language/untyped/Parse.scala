package tapl.language.untyped

import tapl.common._
import tapl.component.varapp

trait Parse[A[-X, Y] <: Alg[X, Y]] extends varapp.Parse[A] {
  lexical.delimiters += ("\\", ".")

  private lazy val pLamE: Parser[Exp[A]] =
    ("\\" ~> lcid) ~ ("." ~> pE) ^^ { case x ~ e0 => CAbs(x, e0) }

  lazy val pUntypedE: Parser[Exp[A]] = pLamE ||| pVarAppE

  override lazy val pE: Parser[Exp[A]] = pUntypedE
}

object Parse extends Parse[Alg]