package inspect.component.nat

import inspect.common._
import inspect.component.nat.Term._

trait Parse[A[-X, Y] <: Term[X, Y]] extends EParser[A] {
  lexical.reserved += ("iszero", "succ", "pred")
  lexical.delimiters += ("(", ")")

  private lazy val pNum = numericLit ^^ (x => {
    def num(x: Int): Exp[A] = x match {
      case 0 => TmZero[A, A]()
      case _ => TmSucc[A, A](num(x - 1))
    }

    num(x.toInt)
  })

  private lazy val pSucc = "succ" ~> pE ^^ TmSucc[A, A]

  private lazy val pPred = "pred" ~> pE ^^ TmPred[A, A]

  private lazy val pIsZero = "iszero" ~> pE ^^ TmIsZero[A, A]

  private lazy val pParen = "(" ~> pE <~ ")"

  lazy val pNatE: Parser[Exp[A]] = pNum ||| pSucc ||| pPred ||| pIsZero ||| pParen
}
