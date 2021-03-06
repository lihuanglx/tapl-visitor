package convert.language.untyped

import convert.common._
import convert.component.varapp
import convert.language.untyped.Term.Factory._

trait Parse[A[-X, Y] <: Term[X, Y]] extends varapp.Parse[A] {
  lexical.delimiters += ("\\", ".")

  private lazy val pLamE: Parser[Exp[A]] =
    ("\\" ~> lcid) ~ ("." ~> pE) ^^ { case x ~ e0 => TmAbs[A, A](x, e0) }

  lazy val pUntypedE: Parser[Exp[A]] = pLamE ||| pVarAppE

  override lazy val pE: Parser[Exp[A]] = pUntypedE
}

object Parse extends Parse[Term]
