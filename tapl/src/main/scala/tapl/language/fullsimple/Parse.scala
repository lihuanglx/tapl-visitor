package tapl.language.fullsimple

import tapl.common._
import tapl.component.{simple, variant}

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends simple.Parse[A, B] with variant.Parse[A, B] {

  lazy val pFullSimpleE: Parser[TExp[A, Exp[B]]] = pSimpleE ||| pVariantE
  lazy val pFullSimpleT: Parser[Exp[B]] = pSimpleT ||| pVariantT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pFullSimpleE
  override lazy val pT: Parser[Exp[B]] = pFullSimpleT
}
