package tapl.component.bottom

import tapl.common._
import tapl.component.bottom.Type.Factory._

trait Parse[A[-X, Y] <: Type[X, Y]] extends TParser[A] {
  lexical.reserved += "Bot"

  lazy val pBottomT: Parser[Exp[A]] = "Bot" ^^ { _ => TyBot[A]() }
}
