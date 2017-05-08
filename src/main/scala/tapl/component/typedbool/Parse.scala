package tapl.component.typedbool

import tapl.common.{EParser, Exp, TParser}
import tapl.component.bool
import tapl.component.typedbool.TFactory._

trait Parse[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends EParser[A] with TParser[B] with bool.Parse[A] {
  lexical.reserved += "Bool"

  lazy val pTypedBoolE: Parser[Exp[A]] = pBoolE
  lazy val pTypedBoolT: Parser[Exp[B]] = "Bool" ^^ { _ => CTyBool[B]() }
}