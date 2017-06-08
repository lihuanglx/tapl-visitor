package tapl.component.variant

import tapl.common._
import tapl.component.variant.Alg.Factory._
import tapl.component.variant.TAlg.Factory._

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends ETParser[A, B] {
  lexical.reserved += ("as", "case", "of")
  lexical.delimiters += ("<", ">", "=", ":", ",", "|", "=>")

  lazy val pVariantE: Parser[TExp[A, Exp[B]]] =
    ("<" ~> lcid) ~ ("=" ~> pE <~ ">") ~ ("as" ~> pT) ^^ { case x ~ e0 ~ t0 => TmTag[A, Exp[B]](x, e0, t0) } |||
      ("case" ~> pE <~ "of") ~ repsep(("<" ~> lcid) ~ ("=" ~> lcid) ~ ((">" ~ "=>") ~> pE) ^^ {
        case x1 ~ x2 ~ e0 => (x1, x2, e0)
      }, "|") ^^ { case e0 ~ l => TmCase[A, Exp[B]](e0, l) }

  lazy val pVariantT: Parser[Exp[B]] =
    "<" ~> repsep(lcid ~ (":" ~> pT) ^^ { case x ~ t0 => (x, t0) }, ",") <~ ">" ^^ TyVariant[B]
}
