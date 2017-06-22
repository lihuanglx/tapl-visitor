import org.scalatest._
import tapl.common._
import tapl.language.fullsimple._

class FullSimple extends FreeSpec with Matchers {

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
      val inp =
        """((\r: {x:Nat, y:Bool}.
          |  \v: <l:Nat, r:Nat>.
          |  case v of <l=lv> => if r.y then lv else r.x
          |          | <r=rv> => rv)
          |{x = 3, y = false})
          |(<l = 5> as <l:Nat, r:Nat>)
        """.stripMargin
      val ast = "TmApp(TmApp(TmAbs(r,TyRecord(List((x,TyNat()), (y,TyBool()))),TmAbs(v,TyVariant(List((l,TyNat()), (r,TyNat()))),TmCase(TmVar(v),List((l,lv,TmIf(TmProj(TmVar(r),y),TmVar(lv),TmProj(TmVar(r),x))), (r,rv,TmVar(rv)))))),TmRecord(List((x,TmSucc(TmSucc(TmSucc(TmZero())))), (y,TmFalse())))),TmTag(l,TmSucc(TmSucc(TmSucc(TmSucc(TmSucc(TmZero()))))),TyVariant(List((l,TyNat()), (r,TyNat())))))"
      val print = "((\\(r:{x: Nat, y: Bool}).\\(v:<l:Nat, r:Nat>).case v of <l=lv> => if (r.y) then (lv) else (r.x) | <r=rv> => rv {x = 3, y = false}) <l=5> as <l:Nat, r:Nat>)"
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
