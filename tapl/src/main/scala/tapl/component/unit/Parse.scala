package tapl.component.unit

import tapl.common._
import tapl.component.unit.Alg.Factory._
import tapl.component.unit.TAlg.Factory._

trait Parse[A[-R, E] <: Alg[R, E], B[-X, Y] <: TAlg[X, Y]] extends EParser[A] with TParser[B] {
  lexical.reserved += ("unit", "Unit")

  lazy val pUnitE: Parser[Exp[A]] = "unit" ^^^ TmUnit[A]()
  lazy val pUnitT: Parser[Exp[B]] = "Unit" ^^^ TyUnit[B]()
}

