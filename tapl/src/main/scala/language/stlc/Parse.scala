package language
package stlc

import gems._
import Term.Factory._
import Type.Factory._

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]] extends BaseParser {
  lexical.reserved += ("unit", "Unit")
  lexical.delimiters += ("\\", ".", "(", ")", ":", "->")

  private lazy val pLambda =
    ("\\" ~> ident) ~ (":" ~> pT) ~ ("." ~> pE) ^^ { case x ~ t0 ~ e0 => TmAbs[A, A[-?, ?, Exp[B]], Exp[B]](x, t0, e0) }

  lazy val pStlcE: PackratParser[Exp2[A, Exp[B]]] =
    pE ~ pE ^^ { case e1 ~ e2 => TmApp[A, A[-?, ?, Exp[B]], Exp[B]](e1, e2) } |||
      pLambda |||
      ident ^^ TmVar[A, A[-?, ?, Exp[B]], Exp[B]] |||
      "unit" ^^^ TmUnit[A, A[-?, ?, Exp[B]], Exp[B]] |||
      "(" ~> pE <~ ")"

  lazy val pStlcT: PackratParser[Exp[B]] =
    pT ~ ("->" ~> pT) ^^ { case t1 ~ t2 => TyArr[B, B](t1, t2) } |||
      "Unit" ^^^ TyUnit[B, B] |||
      "(" ~> pT <~ ")"

  lazy val pE: PackratParser[Exp2[A, Exp[B]]] = pStlcE

  lazy val pT: PackratParser[Exp[B]] = pStlcT
}
