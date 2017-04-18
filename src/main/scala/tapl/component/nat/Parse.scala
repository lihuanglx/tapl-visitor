package tapl.component.nat

import tapl.common.{CommonParser, Exp}


trait Parse[A[-X, Y] <: Alg[X, Y]] extends CommonParser[Exp[A]] {
  lexical.reserved += ("iszero", "succ", "pred")
  lexical.delimiters += ("(", ")")

  val f: Factory[A]

  private lazy val pNum = numericLit ^^ (x => {
    def num(x: Int): Exp[A] = x match {
      case 0 => f.TmZero()
      case _ => f.TmSucc(num(x - 1))
    }

    num(x.toInt)
  })

  private lazy val pSucc = "succ" ~> pE ^^ f.TmSucc

  private lazy val pPred = "pred" ~> pE ^^ f.TmPred

  private lazy val pIsZero = "iszero" ~> pE ^^ f.TmIsZero

  private lazy val pParen = "(" ~> pE <~ ")"

  lazy val pNatE: Parser[Exp[A]] = pNum ||| pSucc ||| pPred ||| pIsZero ||| pParen

  val pE: Parser[Exp[A]]
}