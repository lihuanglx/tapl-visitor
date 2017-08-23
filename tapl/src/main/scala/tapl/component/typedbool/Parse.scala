package tapl.component.typedbool

import tapl.common._
import tapl.component.bool
import tapl.component.typedbool.Type.Factory._

trait Parse[A[-X, Y] <: Term[X, Y], B[-X, Y] <: Type[X, Y]] extends EParser[A] with TParser[B] with bool.Parse[A] {
  lexical.reserved += "Bool"

  lazy val pTypedBoolE: Parser[Exp[A]] = pBoolE
  lazy val pTypedBoolT: Parser[Exp[B]] = "Bool" ^^ { _ => TyBool[B]() }
}
