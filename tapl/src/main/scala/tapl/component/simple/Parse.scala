package tapl.component.simple

import tapl.common._
import tapl.component._
import tapl.language.tyarith
import tapl.component.simple.Alg.Factory._
import tapl.component.simple.TAlg.Factory._

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends tyarith.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B]
    with floatstring.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam]
    with let.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam]
    with typedrecord.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B]
    with typed.Parse[A, B] {

  lexical.reserved += ("unit", "Unit", "as", "fix", "String", "Float", "inert")
  lexical.delimiters += ("(", ")", "[", "]")

  private lazy val pExtensionE: Parser[TExp[A, Exp[B]]] =
    "unit" ^^ { _ => TmUnit[A, Exp[B]]() } |||
      pE ~ ("as" ~> pT) ^^ { case e0 ~ t0 => TmAscribe[A, Exp[B]](e0, t0) } |||
      "fix" ~> pE ^^ TmFix[A, Exp[B]] |||
      "inert" ~> "[" ~> pT <~ "]" ^^ TmInert[A, Exp[B]]

  private lazy val pExtensionT: Parser[Exp[B]] =
    "Unit" ^^^ TyUnit[B]() |||
      "String" ^^^ TyString[B]() |||
      "Float" ^^^ TyFloat[B]()

  lazy val pSimpleE: Parser[TExp[A, Exp[B]]] =
    pTyArithE ||| pTypedE ||| pTypedRecordE ||| pExtensionE ||| pFloatStringE ||| pLetE

  lazy val pSimpleT: Parser[Exp[B]] =
    pTyArithT ||| pTypedT ||| pTypedRecordT ||| pExtensionT
}
