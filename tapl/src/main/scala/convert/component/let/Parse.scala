package convert.component.let

import convert.common._
import convert.component.let.Term._

trait Parse[A[-X, Y] <: Term[X, Y]] extends EParser[A] {
  lexical.reserved += ("let", "in")
  lexical.delimiters += "="

  lazy val pLetE: Parser[Exp[A]] =
    ("let" ~> lcid) ~ ("=" ~> pE) ~ ("in" ~> pE) ^^ { case x ~ e1 ~ e2 => TmLet[A, A](x, e1, e2) }
}
