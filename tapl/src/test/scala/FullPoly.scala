import org.scalatest._
import tapl.common._
import tapl.language.fullpoly._

class FullPoly extends FreeSpec with Matchers {
  type E = Exp2[Term, Exp[Type]]

  type Case = {
    val inp: String
    val ast: String
    val print: String
    val typ: String
    val eval: String
  }

  val cases: List[Case] = List(
    new {
      val inp = "(\\f:Nat->Nat. f 2) ((\\X.\\x:X.x) [Nat])"
      val ast = "TmApp(TmAbs(f,TyArr(TyNat(),TyNat()),TmApp(TmVar(f),TmSucc(TmSucc(TmZero())))),TmTApp(TmTAbs(X,TmAbs(x,TyVar(X),TmVar(x))),TyNat()))"
      val print = "(\\(f:(Nat)->Nat).(f 2) \\X.\\(x:X).x [Nat])"
      val typ = "Nat"
      val eval = "TmSucc(TmSucc(TmZero()))"
    }
  )

  val parse: String => E = new Parse[Term, Type] {}.parse(_).get

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
