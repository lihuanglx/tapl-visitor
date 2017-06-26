import org.scalatest._
import tapl.common._
import tapl.language.fullomega._

import scala.collection.mutable

class FullOmega extends FreeSpec with Matchers {
  type E = Exp3[Alg, Exp2[TAlg, Exp[KAlg]], Exp[KAlg]]

  type Case = {
    val inp: String
    val ast: String
    val print: String
    val typ: String
    val eval: String
  }

  val cases: List[Case] = List(
    new {
      val inp = "let {X,ops} = {*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X:Star, {c:X, f:X->Nat}} in (ops.f ops.c)"
      val ast = "TmUnpack(X,ops,TmPack(TyNat(),TmRecord(List((c,TmZero()), (f,TmAbs(x,TyNat(),TmSucc(TmVar(x)))))),TySome(X,KnStar(),TyRecord(List((c,TyVar(X)), (f,TyArr(TyVar(X),TyNat())))))),TmApp(TmProj(TmVar(ops),f),TmProj(TmVar(ops),c)))"
      val print = "let {X,ops} = {*Nat,{c = 0, f = \\(x:Nat).succ (x)}} as {Some X:Star,{c: X, f: (X)->Nat}} in (ops.f ops.c)"
      val typ = "Nat"
      val eval = "TmSucc(TmZero())"
    }
  )

  val parse: String => E = new Parse[Alg, TAlg, KAlg] {}.parse(_).get

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
        parse(c.inp)(Typer)(Ctx.empty())(Ctx.empty())(Ctx.empty())(TPrint) shouldBe c.typ
      }
    }
  }

  "Eval" - {
    def eval(e: E, c: mutable.MutableList[E]): E = if (e(IsVal)) e else eval(e(Eval)(c), c)

    cases foreach { c =>
      c.inp in {
        eval(parse(c.inp), mutable.MutableList()).toString shouldBe c.eval
      }
    }
  }

}
