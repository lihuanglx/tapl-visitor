package tapl.component.unit

import tapl.common._
import tapl.component.unit.Term.Factory._
import tapl.component.unit.Type.Factory._

trait Parse[A[-R, E] <: Term[R, E], B[-X, Y] <: Type[X, Y]] extends EParser[A] with TParser[B] {
  lexical.reserved += ("unit", "Unit")

  lazy val pUnitE: Parser[Exp[A]] = "unit" ^^^ TmUnit[A]()
  lazy val pUnitT: Parser[Exp[B]] = "Unit" ^^^ TyUnit[B]()
}

