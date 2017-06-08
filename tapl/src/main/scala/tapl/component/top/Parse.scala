package tapl.component.top

import tapl.common._
import tapl.component.top.TAlg.Factory._

trait Parse[A[-X, Y] <: TAlg[X, Y]] extends TParser[A] {
  lexical.reserved += "Top"

  lazy val pTopT: Parser[Exp[A]] = "Top" ^^ { _ => TyTop[A]() }
}
