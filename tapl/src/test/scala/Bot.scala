import tapl.language.bot._
import org.scalatest._
import tapl.common._

class Bot extends FreeSpec with Matchers {

  type E = Exp2[Alg, Exp[TAlg]]

  type Case = {
    val inp: String
    val ast: String
    val print: String
    val typ: String
    val eval: String
  }

  val cases: List[Case] = List(
    new {
      val inp = "(\\x:Bot. x x)"
      val ast = "TmAbs(x,TyBot(),TmApp(TmVar(x),TmVar(x)))"
      val print = "\\(x:Bot).(x x)"
      val typ = "(Bot)->Bot"
      val eval = ast
    },
    new {
      val inp = "(\\x:Top.x) (\\x:Top.x)"
      val ast = "TmApp(TmAbs(x,TyTop(),TmVar(x)),TmAbs(x,TyTop(),TmVar(x)))"
      val print = "(\\(x:Top).x \\(x:Top).x)"
      val typ = "Top"
      val eval = "TmAbs(x,TyTop(),TmVar(x))"
    }
  )

  val parse: String => E = new Parse[Alg, TAlg] {}.parse(_).get

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

  "Type" - {
    cases foreach { c =>
      c.inp in {
        parse(c.inp)(Typer)(Ctx.empty())(TPrint) shouldBe c.typ
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
