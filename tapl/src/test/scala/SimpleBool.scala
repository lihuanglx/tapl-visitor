import org.scalatest._
import tapl.common._
import tapl.language.simplebool._

class SimpleBool extends FreeSpec with Matchers {

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
      val inp = "(\\f:Bool->Bool.f true) (\\x:Bool.if x then false else true)"
      val ast = "TmApp(TmAbs(f,TyArr(TyBool(),TyBool()),TmApp(TmVar(f),TmTrue())),TmAbs(x,TyBool(),TmIf(TmVar(x),TmFalse(),TmTrue())))"
      val print = "(\\(f:(Bool)->Bool).(f true) \\(x:Bool).if (x) then (false) else (true))"
      val typ = "Bool"
      val eval = "TmFalse()"
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
