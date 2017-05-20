package tapl.component.typed

import tapl.common._
import tapl.component.varapp

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends ETParser[A, B]
  with varapp.Parse[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam] {

  lexical.delimiters += ("\\", ".", "(", ")", ":", "->")

  private lazy val pAbsE: Parser[E3[A, Exp[B]]] =
    ("\\" ~> lcid) ~ (":" ~> pT) ~ ("." ~> pE) ^^ { case x ~ t0 ~ e0 => CAbs[A, Exp[B]](x, t0, e0) }

  lazy val pTypedE: Parser[E3[A, Exp[B]]] = pVarAppE ||| pAbsE

  lazy val pTypedT: Parser[Exp[B]] =
    pT ~ ("->" ~> pT) ^^ { case t1 ~ t2 => CTyArr(t1, t2) } |||
      "(" ~> pT <~ ")"
}
