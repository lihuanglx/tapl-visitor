package tapl.component.typevar

import tapl.common._
import tapl.component.typevar.TAlg.Factory._

trait Parse[A[-X, Y] <: TAlg[X, Y]] extends TParser[A] {
  lazy val pTypeVarT: Parser[Exp[A]] = ucid ^^ TyVar[A]
}
