package tapl.component.typevar

import tapl.common._

trait Parse[A[-X, Y] <: TAlg[X, Y]] extends TParser[A] {
  lazy val pTypeVarT: Parser[Exp[A]] = ucid ^^ CTyVar[A]
}
