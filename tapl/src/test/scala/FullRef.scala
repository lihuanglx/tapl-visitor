import tapl.language.fullref._
import org.scalatest._
import tapl.common._

import scala.collection.mutable

class FullRef extends FreeSpec with Matchers {

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
      val inp = "(\\r:(Source Bool). if !r then 1 else 2) (ref false)"
      val ast = "TmApp(TmAbs(r,TySource(TyBool()),TmIf(TmDeRef(TmVar(r)),TmSucc(TmZero()),TmSucc(TmSucc(TmZero())))),TmRef(TmFalse()))"
      val print = "(\\(r:Source Bool).if (!r) then (1) else (2) ref false)"
      val typ = "Nat"
      val eval = "TmSucc(TmSucc(TmZero()))"
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
        parse(c.inp)(Typer)(Ctx.empty())(Ctx.empty())(TPrint) shouldBe c.typ
      }
    }
  }

  "Eval" - {
    def eval(e: E, c: mutable.MutableList[Exp2[Alg, Exp[TAlg]]]): E =
      if (e(IsVal)) e else eval(e(Eval)(c), c)

    cases foreach { c =>
      c.inp in {
        eval(parse(c.inp), mutable.MutableList()).toString shouldBe c.eval
      }
    }
  }

}
