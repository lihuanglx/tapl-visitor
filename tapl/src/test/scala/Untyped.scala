import org.scalatest._
import tapl.common._
import tapl.language.untyped._

class Untyped extends FreeSpec with Matchers {

  type E = Exp[Alg]

  type Case = {
    val inp: String
    val ast: String
    val print: String
    val eval: String
  }

  val cases: List[Case] = List(
    new {
      val inp = "\\x.x (\\y.y)"
      val ast = "CAbs(x,CApp(CVar(x),CAbs(y,CVar(y))))"
      val print = "\\x.(x \\y.y)"
      val eval = ast
    },
    new {
      val inp = "(\\x.(\\x.x)) (\\y.y)"
      val ast = "CApp(CAbs(x,CAbs(x,CVar(x))),CAbs(y,CVar(y)))"
      val print = "(\\x.\\x.x \\y.y)"
      val eval = "CAbs(x,CVar(x))"
    }
  )

  val parse: String => E = Parse.parse(_).get

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