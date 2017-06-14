package tapl.language.fullref

import tapl.common._
import tapl.component.{variant, ref}
import tapl.language.fullsub
import tapl.language.fullref.TAlg.Factory._

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends fullsub.Parse[A, B] with variant.Parse[A, B]
    with ref.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  lexical.reserved += ("Source", "Sink")

  lazy val pFullRefE: Parser[TExp[A, Exp[B]]] = pFullSubE ||| pRefE ||| pVariantE
  lazy val pFullRefT: Parser[Exp[B]] =
    pFullSubT ||| pRefT ||| pTopT |||
      "Source" ~> pT ^^ TySource[B] |||
      "Sink" ~> pT ^^ TySink[B]

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pFullRefE
  override lazy val pT: Parser[Exp[B]] = pFullRefT
}
