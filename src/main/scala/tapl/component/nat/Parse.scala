package tapl.component.nat

import tapl.common._

trait Parse[A[-X, Y] <: Alg[X, Y]] extends EParser[A] {
  lexical.reserved += ("iszero", "succ", "pred")
  lexical.delimiters += ("(", ")")

  private lazy val pNum = numericLit ^^ (x => {
    def num(x: Int): Exp[A] = x match {
      case 0 => CZero[A]()
      case _ => CSucc[A](num(x - 1))
    }

    num(x.toInt)
  })

  private lazy val pSucc = "succ" ~> pE ^^ CSucc[A]

  private lazy val pPred = "pred" ~> pE ^^ CPred[A]

  private lazy val pIsZero = "iszero" ~> pE ^^ CIsZero[A]

  private lazy val pParen = "(" ~> pE <~ ")"

  lazy val pNatE: Parser[Exp[A]] = pNum ||| pSucc ||| pPred ||| pIsZero ||| pParen
}