package language
package boolstlc

import gems._

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends stlc.Parse[A, B] with bool.Parse[A[-?, ?, Exp[B]], B] {

  lazy val pBoolstlcE: PackratParser[Exp2[A, Exp[B]]] = pBoolE ||| pStlcE
  lazy val pBoolstlcT: PackratParser[Exp[B]] = pBoolT ||| pStlcT

  override lazy val pE: PackratParser[Exp2[A, Exp[B]]] = pBoolstlcE
  override lazy val pT: PackratParser[Exp[B]] = pBoolstlcT
}
