package tapl.component.ref

import tapl.common._
import tapl.component.ref.Term.Factory._
import tapl.component.ref.Type.Factory._

trait Parse[A[-X, Y] <: Term[X, Y], B[-X, Y] <: Type[X, Y]] extends EParser[A] with TParser[B] {
  lexical.reserved += ("ref", "Ref")
  lexical.delimiters += ("!", ":=")

  lazy val pRefE: Parser[Exp[A]] = {
    "ref" ~> pE ^^ TmRef[A] |||
      "!" ~> pE ^^ TmDeRef[A] |||
      pE ~ (":=" ~> pE) ^^ { case lhs ~ rhs => TmAssign(lhs, rhs) }
  }

  lazy val pRefT: Parser[Exp[B]] = "Ref" ~> pT ^^ TyRef[B]
}
