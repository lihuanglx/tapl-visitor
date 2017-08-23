package tapl.component.top

import tapl.common._
import tapl.component.top.Type.Factory._

trait Parse[A[-X, Y] <: Type[X, Y]] extends TParser[A] {
  lexical.reserved += "Top"

  lazy val pTopT: Parser[Exp[A]] = "Top" ^^ { _ => TyTop[A]() }
}
