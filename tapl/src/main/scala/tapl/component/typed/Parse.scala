package tapl.component.typed

import tapl.common._
import tapl.component.varapp
import tapl.component.typed.Alg.Factory._
import tapl.component.typed.TAlg.Factory._

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends ETParser[A, B]
  with varapp.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam] {

  lexical.delimiters += ("\\", ".", "(", ")", ":", "->", "$")

  private lazy val pAbsE: Parser[TExp[A, Exp[B]]] =
    ("\\" ~> lcid) ~ (":" ~> pT) ~ ("." ~> pE) ^^ { case x ~ t0 ~ e0 => TmAbs[A, Exp[B]](x, t0, e0) }

  lazy val pTypedE: Parser[TExp[A, Exp[B]]] = pVarAppE ||| pAbsE

  lazy val pTypedT: Parser[Exp[B]] =
    pT ~ ("->" ~> pT) ^^ { case t1 ~ t2 => TyArr(t1, t2) } |||
      ("$" ~> ucid) ^^ TyId[B] |||
      "(" ~> pT <~ ")"
}
