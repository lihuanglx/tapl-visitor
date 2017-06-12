package tapl.component.ref

import tapl.common._
import tapl.component.ref.Alg.Factory._
import tapl.component.ref.TAlg.Factory._

trait Parse[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends EParser[A] with TParser[B] {
  lexical.reserved += ("ref", "Ref")
  lexical.delimiters += ("!", ":=", "@")

  lazy val pRefE: Parser[Exp[A]] = {
    "ref" ~> pE ^^ TmRef[A] |||
      "!" ~> pE ^^ TmDeRef[A] |||
      pE ~ (":=" ~> pE) ^^ { case lhs ~ rhs => TmAssign(lhs, rhs) } |||
      "@" ~> lcid ^^ TmLoc[A]
  }

  lazy val pRefT: Parser[Exp[B]] = "Ref" ~> pT ^^ TyRef[B]
}
