package tapl.language.fullequirec

import tapl.common._
import tapl.component.{extension, variant}
import tapl.language.equirec

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends equirec.Parse[A, B] with extension.Parse[A, B] with variant.Parse[A, B] {

  lazy val pFullEquiRecE: Parser[TExp[A, Exp[B]]] = pEquiRecE ||| pExtensionE ||| pVariantE
  lazy val pFullEquiRecT: Parser[Exp[B]] = pEquiRecT ||| pExtensionT ||| pVariantT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pFullEquiRecE
  override lazy val pT: Parser[Exp[B]] = pFullEquiRecT
}
