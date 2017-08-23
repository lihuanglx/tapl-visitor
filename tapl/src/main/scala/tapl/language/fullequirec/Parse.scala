package tapl.language.fullequirec

import tapl.common._
import tapl.component.{extension, variant}
import tapl.language.equirec

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends equirec.Parse[A, B] with extension.Parse[A, B] with variant.Parse[A, B] {

  lazy val pFullEquiRecE: Parser[Exp2[A, Exp[B]]] = pEquiRecE ||| pExtensionE ||| pVariantE
  lazy val pFullEquiRecT: Parser[Exp[B]] = pEquiRecT ||| pExtensionT ||| pVariantT

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pFullEquiRecE
  override lazy val pT: Parser[Exp[B]] = pFullEquiRecT
}
