import org.scalatest._
import tapl.common._
import tapl.language.arith._

class Arith extends FreeSpec with Matchers {
  type E = Exp[Alg]

  type Case = {
    val inp: String
    val ast: String
    val print: String
    val eval: String
  }

  val cases: List[Case] = List(
    new {
      val inp = "if true then (if false then 2 else (pred 4)) else 3"
      val ast = "TmIf(TmTrue(),TmIf(TmFalse(),TmSucc(TmSucc(TmZero())),TmPred(TmSucc(TmSucc(TmSucc(TmSucc(TmZero())))))),TmSucc(TmSucc(TmSucc(TmZero()))))"
      val print = "if (true) then (if (false) then (2) else (pred (4))) else (3)"
      val eval = "TmSucc(TmSucc(TmSucc(TmZero())))"
    }
  )

  val parse: String => E = new Parse[Alg] {}.parse(_).get

  "Parse" - {
    cases foreach { c =>
      c.inp in {
        parse(c.inp).toString shouldBe c.ast
      }
    }
  }

  "Print" - {
    cases foreach { c =>
      c.inp in {
        parse(c.inp)(Print) shouldBe c.print
      }
    }
  }

  "Eval" - {
    def eval(e: E): E = if (e(IsVal)) e else eval(e(Eval))

    cases foreach { c =>
      c.inp in {
        eval(parse(c.inp)).toString shouldBe c.eval
      }
    }
  }
}
