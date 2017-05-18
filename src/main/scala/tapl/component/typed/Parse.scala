package tapl.component.typed

import tapl.common.Util.E3
import tapl.common.{ETParser, Exp}
import tapl.component.typed.Factory._
import tapl.component.typed.TFactory._
import tapl.component.varapp

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends ETParser[A, B]
  with varapp.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam] {

  lexical.delimiters += ("\\", ".", "(", ")", ":", "->")

  private val pAbsE: Parser[E3[A, Exp[B]]] =
    ("\\" ~> lcid) ~ (":" ~> pT) ~ ("." ~> pE) ^^ { case x ~ t0 ~ e0 => CAbs[A, Exp[B]](x, t0, e0) }

  val pTypedE: Parser[E3[A, Exp[B]]] = pVarAppE ||| pAbsE

  val pTypedT: Parser[Exp[B]] =
    pT ~ ("->" ~> pT) ^^ { case t1 ~ t2 => CTyArr(t1, t2) } |||
      "(" ~> pT <~ ")"
}
