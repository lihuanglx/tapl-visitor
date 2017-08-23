package tapl.component.variant

import tapl.common._
import tapl.component.variant.Term.Factory._
import tapl.component.variant.Type.Factory._

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]] extends ETParser[A, B] {
  lexical.reserved += ("as", "case", "of")
  lexical.delimiters += ("<", ">", "=", ":", ",", "|", "=>")

  lazy val pVariantE: Parser[Exp2[A, Exp[B]]] =
    ("<" ~> lcid) ~ ("=" ~> pE <~ ">") ~ ("as" ~> pT) ^^ { case x ~ e0 ~ t0 => TmTag[A, Exp[B]](x, e0, t0) } |||
      ("case" ~> pE <~ "of") ~ repsep(("<" ~> lcid) ~ ("=" ~> lcid) ~ ((">" ~ "=>") ~> pE) ^^ {
        case x1 ~ x2 ~ e0 => (x1, x2, e0)
      }, "|") ^^ { case e0 ~ l => TmCase[A, Exp[B]](e0, l) }

  lazy val pVariantT: Parser[Exp[B]] =
    "<" ~> repsep(lcid ~ (":" ~> pT) ^^ { case x ~ t0 => (x, t0) }, ",") <~ ">" ^^ TyVariant[B]
}
