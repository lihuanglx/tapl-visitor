import tapl.language.fullerror._
import tapl.common._
import org.scalatest._

class FullError extends FreeSpec with Matchers {

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
      val inp = "(\\f:Bool->Bool. try (f true) with (error false)) (\\x:Bool.if x then error else true)"
      val ast = "TmApp(TmAbs(f,TyArr(TyBool(),TyBool()),TmTry(TmApp(TmVar(f),TmTrue()),TmApp(TmError(),TmFalse()))),TmAbs(x,TyBool(),TmIf(TmVar(x),TmError(),TmTrue())))"
      val print = "(\\(f:(Bool)->Bool).try (f true) with (error false) \\(x:Bool).if (x) then (error) else (true))"
      val typ = "Bool"
      val eval = "TmError()"
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
