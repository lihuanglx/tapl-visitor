package tapl.language.fullsub

import tapl.common._
import tapl.component.{typed, extension, top}

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends typed.Parse[A, B] with extension.Parse[A, B] with top.Parse[B] {

  lazy val pFullSubE: Parser[TExp[A, Exp[B]]] = pTypedE ||| pExtensionE
  lazy val pFullSubT: Parser[Exp[B]] = pTypedT ||| pExtensionT ||| pTopT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pFullSubE
  override lazy val pT: Parser[Exp[B]] = pFullSubT
}
