package tapl.component.pack

import tapl.common._
import tapl.component.pack.Term.Factory._

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-F, T]] extends ETParser[A, B] {
  lexical.reserved += ("as", "let", "in")
  lexical.delimiters += (",", "{", "}", "*", "=")

  lazy val pPackE: Parser[Exp2[A, Exp[B]]] =
    ("{" ~> "*" ~> pT ~ ("," ~> pE) <~ "}") ~ ("as" ~> pT) ^^ { case t1 ~ ex ~ t2 => TmPack(t1, ex, t2) } |||
      "let" ~> ("{" ~> ucid ~ ("," ~> lcid) <~ "}") ~ ("=" ~> pE) ~ ("in" ~> pE) ^^ {
        case tx ~ x ~ e1 ~ e2 => TmUnpack(tx, x, e1, e2)
      }
}
