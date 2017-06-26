import org.scalatest._
import tapl.common._
import tapl.language.fullrecon._

class FullRecon extends FreeSpec with Matchers {
  type E = Exp2[Alg, Exp[TAlg]]

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
      val inp =
        """ let double = \f. \a. (f (f a)) in
          | let a = (double (\x:Nat. succ (succ x))) 1 in
          | let b = (double (\x:Bool. x)) true in
          | if b then a else 0
        """.stripMargin
      val ast = "TmLet(double,TmUAbs(f,TmUAbs(a,TmApp(TmVar(f),TmApp(TmVar(f),TmVar(a))))),TmLet(a,TmApp(TmApp(TmVar(double),TmAbs(x,TyNat(),TmSucc(TmSucc(TmVar(x))))),TmSucc(TmZero())),TmLet(b,TmApp(TmApp(TmVar(double),TmAbs(x,TyBool(),TmVar(x))),TmTrue()),TmIf(TmVar(b),TmVar(a),TmZero()))))"
      val print = "let double = \\f.\\a.(f (f a)) in let a = ((double \\(x:Nat).succ (succ (x))) 1) in let b = ((double \\(x:Bool).x) true) in if (b) then (a) else (0)"
      val typ = "Nat"
      val ast2 = print
      val eval = "TmSucc(TmSucc(TmSucc(TmSucc(TmSucc(TmZero())))))"
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
        val map2 = new Alg.Map2[Alg, Exp[TAlg]] with Impl[(Exp[TAlg] => Exp[TAlg]) => Exp2[Alg, Exp[TAlg]]]
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
