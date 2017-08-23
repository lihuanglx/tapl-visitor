import org.scalatest._
import tapl.common._
import tapl.language.untyped._

class Untyped extends FreeSpec with Matchers {

  type E = Exp[Term]

  type Case = {
    val inp: String
    val ast: String
    val print: String
    val eval: String
  }

  val cases: List[Case] = List(
    new {
      val inp = "\\x.x (\\y.y)"
      val ast = "TmAbs(x,TmApp(TmVar(x),TmAbs(y,TmVar(y))))"
      val print = "\\x.(x \\y.y)"
      val eval = ast
    },
    new {
      val inp = "(\\x.(\\x.x)) (\\y.y)"
      val ast = "TmApp(TmAbs(x,TmAbs(x,TmVar(x))),TmAbs(y,TmVar(y)))"
      val print = "(\\x.\\x.x \\y.y)"
      val eval = "TmAbs(x,TmVar(x))"
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
