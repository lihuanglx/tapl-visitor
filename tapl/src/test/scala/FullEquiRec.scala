import org.scalatest._
import tapl.common._
import tapl.language.fullequirec._

class FullEquiRec extends FreeSpec with Matchers {
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
      val inp = "\\f:T->T.((\\x:(Rec A.A->T). f (x x)) (\\x:(Rec A.A->T). f (x x)))"
      val ast = "TmAbs(f,TyArr(TyVar(T),TyVar(T)),TmApp(TmAbs(x,TyRec(A,TyArr(TyVar(A),TyVar(T))),TmApp(TmVar(f),TmApp(TmVar(x),TmVar(x)))),TmAbs(x,TyRec(A,TyArr(TyVar(A),TyVar(T))),TmApp(TmVar(f),TmApp(TmVar(x),TmVar(x))))))"
      val print = "\\(f:(T)->T).(\\(x:(Rec A.(A)->T)).(f (x x)) \\(x:(Rec A.(A)->T)).(f (x x)))"
      val typ = "((T)->T)->T"
      val eval = ast
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
