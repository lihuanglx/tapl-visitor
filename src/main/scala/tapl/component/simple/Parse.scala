package tapl.component.simple

import tapl.common._
import tapl.component._
import tapl.language.tyarith

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends tyarith.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B]
    with floatstring.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam]
    with let.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam]
    with typedrecord.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B]
    with typed.Parse[A, B] with typevar.Parse[B] {

  lexical.reserved += ("unit", "Unit", "as", "fix", "String", "Float", "inert")
  lexical.delimiters += ("(", ")", "[", "]")

  private lazy val pExtensionE: Parser[E3[A, Exp[B]]] =
    "unit" ^^ { _ => CUnit[A, Exp[B]]() } |||
      pE ~ ("as" ~> pT) ^^ { case e0 ~ t0 => CAscribe[A, Exp[B]](e0, t0) } |||
      "fix" ~> pE ^^ CFix[A, Exp[B]] |||
      "inert" ~> "[" ~> pT <~ "]" ^^ CInert[A, Exp[B]]

  private lazy val pExtensionT: Parser[Exp[B]] =
    "Unit" ^^^ CTyUnit[B]() |||
      "String" ^^^ CTyString[B]() |||
      "Float" ^^^ CTyFloat[B]()

  lazy val pSimpleE: Parser[E3[A, Exp[B]]] =
    pTyArithE ||| pTypedE ||| pTypedRecordE ||| pExtensionE ||| pFloatStringE ||| pLetE

  lazy val pSimpleT: Parser[Exp[B]] =
    pTyArithT ||| pTypedT ||| pTypedRecordT ||| pExtensionT ||| pTypeVarT
}
