package tapl.component.typedbool

import tapl.common.{ETParser, Exp}
import tapl.component.bool
import tapl.component.typedbool.TFactory._

trait Parse[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends ETParser[A, B] with bool.Parse[A] {
  lexical.reserved += "Bool"

  val pTypedBoolE: Parser[Exp[A]] = pBoolE
  val pTypedBoolT: Parser[Exp[B]] = "Bool" ^^ { _ => CTyBool[B]() }
}