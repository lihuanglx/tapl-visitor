package tapl.language.simplebool

import tapl.common._
import tapl.component.{typed, typedbool}

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]] extends typed.Parse[A, B]
  with typedbool.Parse[A[-?, ?, Exp[B]], B] {

  lazy val pSimpleBoolE: Parser[Exp2[A, Exp[B]]] = pTypedE ||| pTypedBoolE
  lazy val pSimpleBoolT: Parser[Exp[B]] = pTypedT ||| pTypedBoolT

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pSimpleBoolE
  override lazy val pT: Parser[Exp[B]] = pSimpleBoolT
}
