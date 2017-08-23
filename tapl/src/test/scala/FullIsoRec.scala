import org.scalatest._
import tapl.common._
import tapl.language.fullisorec._

class FullIsoRec extends FreeSpec with Matchers {
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
      val inp = "\\x: (Rec A.A->T). unfold [Rec A.A->T] x"
      val ast = "TmAbs(x,TyRec(A,TyArr(TyVar(A),TyVar(T))),TmUnfold(TmVar(x),TyRec(A,TyArr(TyVar(A),TyVar(T)))))"
      val print = "\\(x:(Rec A.(A)->T)).unfold [(Rec A.(A)->T)] x"
      val typ = "((Rec A.(A)->T))->((Rec A.(A)->T))->T"
      val eval = ast
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
