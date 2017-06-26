import tapl.common._
import org.scalatest._
import tapl.language.fullsub._

class FullSub extends FreeSpec with Matchers {

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
      val inp = "(\\r: {x: Nat}. r.x) if true then {x = 3, y = true} else {x = 5, y = false, z = 1}"
      val ast = "TmApp(TmAbs(r,TyRecord(List((x,TyNat()))),TmProj(TmVar(r),x)),TmIf(TmTrue(),TmRecord(List((x,TmSucc(TmSucc(TmSucc(TmZero())))), (y,TmTrue()))),TmRecord(List((x,TmSucc(TmSucc(TmSucc(TmSucc(TmSucc(TmZero())))))), (y,TmFalse()), (z,TmSucc(TmZero()))))))"
      val print = "(\\(r:{x: Nat}).r.x if (true) then ({x = 3, y = true}) else ({x = 5, y = false, z = 1}))"
      val typ = "Nat"
      val eval = "TmSucc(TmSucc(TmSucc(TmZero())))"
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
