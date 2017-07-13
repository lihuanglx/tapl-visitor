package tapl.component.extension

import tapl.common._
import tapl.component._
import tapl.language.tyarith
import tapl.component.extension.Alg.Factory._
import tapl.component.extension.TAlg.Factory._

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends tyarith.Parse[A[-?, ?, Exp[B]], B] with floatstring.Parse[A[-?, ?, Exp[B]]]
    with let.Parse[A[-?, ?, Exp[B]]] with typedrecord.Parse[A[-?, ?, Exp[B]], B]
    with unit.Parse[A[-?, ?, Exp[B]], B] {

  lexical.reserved += ("as", "fix", "String", "Float", "inert")
  lexical.delimiters += ("(", ")", "[", "]")

  private lazy val pExtE: Parser[Exp2[A, Exp[B]]] =
    pE ~ ("as" ~> pT) ^^ { case e0 ~ t0 => TmAscribe[A, Exp[B]](e0, t0) } |||
      "fix" ~> pE ^^ TmFix[A, Exp[B]]

  private lazy val pExtT: Parser[Exp[B]] =
    "String" ^^^ TyString[B]() |||
      "Float" ^^^ TyFloat[B]()

  lazy val pExtensionE: Parser[Exp2[A, Exp[B]]] =
    pTyArithE ||| pTypedRecordE ||| pFloatStringE ||| pLetE ||| pExtE ||| pUnitE

  lazy val pExtensionT: Parser[Exp[B]] =
    pTyArithT ||| pTypedRecordT ||| pExtT ||| pUnitT
}

