import org.scalatest._
import tapl.common._
import tapl.language.recon._

class Recon extends FreeSpec with Matchers {
  type E = Exp2[Term, Exp[Type]]

  type Case = {
    val inp: String
    val ast: String
    val print: String
    val typ: String
    val ast2: String
    val eval: String
  }

  val cases: List[Case] = List(
    new {
      val inp = "((\\f:Y. \\a:X. f (f a)) (\\b:Bool. if b then false else true)) true"
      val ast = "TmApp(TmApp(TmAbs(f,TyVar(Y),TmAbs(a,TyVar(X),TmApp(TmVar(f),TmApp(TmVar(f),TmVar(a))))),TmAbs(b,TyBool(),TmIf(TmVar(b),TmFalse(),TmTrue()))),TmTrue())"
      val print = "((\\(f:Y).\\(a:X).(f (f a)) \\(b:Bool).if (b) then (false) else (true)) true)"
      val typ = "Bool"
      val ast2 = "((\\(f:(Bool)->Bool).\\(a:Bool).(f (f a)) \\(b:Bool).if (b) then (false) else (true)) true)"
      val eval = "TmTrue()"
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
        val (ty, _, cs) = parse(c.inp)(Typer)(Ctx.empty(), 0)
        Unify(ty, cs)(TPrint) shouldBe c.typ
      }
    }
  }

  "Type reconstruction" - {
    cases foreach { c =>
      c.inp in {
        val (ty, _, cs) = parse(c.inp)(Typer)(Ctx.empty(), 0)
        val solution = Unify.unify(cs)
        val map2 = new Term.Map2[Term, Exp[Type]] with Impl[(Exp[Type] => Exp[Type]) => Exp2[Term, Exp[Type]]]
        parse(c.inp)(map2)(_ (new TSubstImpl(solution)))(Print) shouldBe c.ast2
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
