package tapl.component.variant

import tapl.common.Util.E3
import tapl.common.{ETParser, Exp}

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends ETParser[A, B] {
  lexical.reserved += ("as", "case", "of")
  lexical.delimiters += ("<", ">", "=", ":", ",", "|", "=>")

  lazy val pVariantE: Parser[E3[A, Exp[B]]] =
    ("<" ~> lcid) ~ ("=" ~> pE <~ ">") ~ ("as" ~> pT) ^^ { case x ~ e0 ~ t0 => CTag[A, Exp[B]](x, e0, t0) } |||
      ("case" ~> pE <~ "of") ~ repsep(("<" ~> lcid) ~ ("=" ~> lcid) ~ ((">" ~ "=>") ~> pE) ^^ {
        case x1 ~ x2 ~ e0 => (x1, x2, e0)
      }, "|") ^^ { case e0 ~ l => CCase[A, Exp[B]](e0, l) }

  lazy val pVariantT: Parser[Exp[B]] =
    "<" ~> repsep(lcid ~ (":" ~> pT) ^^ { case x ~ t0 => (x, t0) }, ",") <~ ">" ^^ CTyVariant[B]
}
