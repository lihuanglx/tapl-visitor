package tapl.language.fullsub

import tapl.common._
import tapl.component.{typed, extension, top}

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends typed.Parse[A, B] with extension.Parse[A, B] with top.Parse[B] {

  lazy val pFullSubE: Parser[Exp2[A, Exp[B]]] = pTypedE ||| pExtensionE
  lazy val pFullSubT: Parser[Exp[B]] = pTypedT ||| pExtensionT ||| pTopT

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pFullSubE
  override lazy val pT: Parser[Exp[B]] = pFullSubT
}
