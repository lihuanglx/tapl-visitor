import org.scalatest._
import tapl.common._
import tapl.language.fulluntyped._

class FullUntyped extends FreeSpec with Matchers {
  type E = Exp[Alg]

  type Case = {
    val inp: String
    val ast: String
    val print: String
    val eval: String
  }

  val cases: List[Case] = List(
    new {
      val inp = "let t = {l=\\x.x, r=(\\x.x)(\\x.x)} in (t.r 2)"
      val ast = "TmLet(t,TmRecord(List((l,TmAbs(x,TmVar(x))), (r,TmApp(TmAbs(x,TmVar(x)),TmAbs(x,TmVar(x)))))),TmApp(TmProj(TmVar(t),r),TmSucc(TmSucc(TmZero()))))"
      val print = "let t = {l = \\x.x, r = (\\x.x \\x.x)} in (t.r 2)"
      val eval = "TmSucc(TmSucc(TmZero()))"
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
