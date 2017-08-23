package tapl.language.fullsimple

import tapl.common._
import tapl.component.{typed, extension, variant}

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends typed.Parse[A, B] with extension.Parse[A, B] with variant.Parse[A, B] {

  lazy val pFullSimpleE: Parser[Exp2[A, Exp[B]]] = pTypedE ||| pExtensionE ||| pVariantE
  lazy val pFullSimpleT: Parser[Exp[B]] = pTypedT ||| pExtensionT ||| pVariantT

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pFullSimpleE
  override lazy val pT: Parser[Exp[B]] = pFullSimpleT
}
