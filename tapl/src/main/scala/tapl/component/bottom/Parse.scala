package tapl.component.bottom

import tapl.common._
import tapl.component.bottom.TAlg.Factory._

trait Parse[A[-X, Y] <: TAlg[X, Y]] extends TParser[A]  {
  lexical.reserved += "Bot"

  lazy val pBottomT: Parser[Exp[A]] = "Bot" ^^ { _ => TyBot[A]() }
}
