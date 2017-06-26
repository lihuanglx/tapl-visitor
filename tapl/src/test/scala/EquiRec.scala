import org.scalatest._
import tapl.common._
import tapl.language.equirec._

class EquiRec extends FreeSpec with Matchers {
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
      val inp = "\\f:((((Rec X.X->A)->A)->A)->A)->T. (\\x:(Rec X.X->A). f x)"
      val ast = "TmAbs(f,TyArr(TyArr(TyArr(TyArr(TyRec(X,TyArr(TyVar(X),TyVar(A))),TyVar(A)),TyVar(A)),TyVar(A)),TyVar(T)),TmAbs(x,TyRec(X,TyArr(TyVar(X),TyVar(A))),TmApp(TmVar(f),TmVar(x))))"
      val print = "\\(f:(((((Rec X.(X)->A))->A)->A)->A)->T).\\(x:(Rec X.(X)->A)).(f x)"
      val typ = "((((((Rec X.(X)->A))->A)->A)->A)->T)->((Rec X.(X)->A))->T"
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
