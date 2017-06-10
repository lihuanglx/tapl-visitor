package tapl.component.extension

import tapl.common._
import tapl.component._
import tapl.language.tyarith
import tapl.component.extension.Alg.Factory._
import tapl.component.extension.TAlg.Factory._

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends tyarith.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B]
    with floatstring.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam]
    with let.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam]
    with typedrecord.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  lexical.reserved += ("unit", "Unit", "as", "fix", "String", "Float", "inert")
  lexical.delimiters += ("(", ")", "[", "]")

  private lazy val pExtE: Parser[TExp[A, Exp[B]]] =
    "unit" ^^ { _ => TmUnit[A, Exp[B]]() } |||
      pE ~ ("as" ~> pT) ^^ { case e0 ~ t0 => TmAscribe[A, Exp[B]](e0, t0) } |||
      "fix" ~> pE ^^ TmFix[A, Exp[B]]

  private lazy val pExtT: Parser[Exp[B]] =
    "Unit" ^^^ TyUnit[B]() |||
      "String" ^^^ TyString[B]() |||
      "Float" ^^^ TyFloat[B]()

  lazy val pExtensionE: Parser[TExp[A, Exp[B]]] =
    pTyArithE ||| pTypedRecordE ||| pFloatStringE ||| pLetE ||| pExtE

  lazy val pExtensionT: Parser[Exp[B]] =
    pTyArithT ||| pTypedRecordT ||| pExtT
}

