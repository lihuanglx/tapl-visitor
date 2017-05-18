package tapl.component.typed2

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.typed
import tapl.component.typed2.TFactory._

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends typed.Parse[A, B] {
  lazy val pTyped2E: Parser[E3[A, Exp[B]]] = pTypedE

  lazy val pTyped2T: Parser[Exp[B]] = pTypedT ||| ucid ^^ CTyVar[B]
}
