package tapl.language.fullref

import tapl.common._
import tapl.component.{variant, ref}
import tapl.language.fullsub
import tapl.language.fullref.Type.Factory._

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends fullsub.Parse[A, B] with variant.Parse[A, B] with ref.Parse[A[-?, ?, Exp[B]], B] {

  lexical.reserved += ("Source", "Sink")

  lazy val pFullRefE: Parser[Exp2[A, Exp[B]]] = pFullSubE ||| pRefE ||| pVariantE
  lazy val pFullRefT: Parser[Exp[B]] =
    pFullSubT ||| pRefT ||| pTopT ||| pVariantT |||
      "Source" ~> pT ^^ TySource[B] |||
      "Sink" ~> pT ^^ TySink[B]

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pFullRefE
  override lazy val pT: Parser[Exp[B]] = pFullRefT
}
