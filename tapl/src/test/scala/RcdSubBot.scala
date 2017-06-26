import tapl.common._
import org.scalatest._
import tapl.language.rcdsubbot._

class RcdSubBot extends FreeSpec with Matchers {

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
      val inp = "\\y:Bot. y.label"
      val ast = "TmAbs(y,TyBot(),TmProj(TmVar(y),label))"
      val print = "\\(y:Bot).y.label"
      val typ = "(Bot)->Bot"
      val eval = ast
    },
    new {
      val inp = "(\\r:{x:Top, y:Top}. r.x) {x = (\\x:Top.x), y = (\\y:Bot.y), z = (\\z:Top.z)}"
      val ast = "TmApp(TmAbs(r,TyRecord(List((x,TyTop()), (y,TyTop()))),TmProj(TmVar(r),x)),TmRecord(List((x,TmAbs(x,TyTop(),TmVar(x))), (y,TmAbs(y,TyBot(),TmVar(y))), (z,TmAbs(z,TyTop(),TmVar(z))))))"
      val print = "(\\(r:{x: Top, y: Top}).r.x {x = \\(x:Top).x, y = \\(y:Bot).y, z = \\(z:Top).z})"
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
