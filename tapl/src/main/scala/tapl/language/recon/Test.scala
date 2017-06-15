package tapl.language.recon

import tapl.common._

object Test {
  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    //λf:Y. λa:X. f (f a)
    val input = "(\\f:$Y. \\a:$X. f (f a)) (\\b:Bool. if b then false else true)"
    val ast: TExp[Alg, Exp[TAlg]] = parser.parse(input).get

    val (ty, _, cs) = ast(Typer)(Ctx.empty(), 0)
    val solution = Unify.unify(cs)

    println("Type: " + Unify(ty, cs)(TPrint))

    val ast2 = ast(new Alg.MapSnd[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
      override def mp(t: Exp[TAlg]): Exp[TAlg] = t(new TSubstImpl(solution))
    })
    go(ast2, 1)
  }

  def go(e: TExp[Alg, Exp[TAlg]], step: Int): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(Print))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }
  }
}

