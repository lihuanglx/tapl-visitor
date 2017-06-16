package tapl.language.fullrecon

import tapl.common._

object Test {
  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    val input =
      """ let double = \f. \a. f(f(a)) in
        | let a = (double (\x:Nat. succ (succ x))) 1 in
        | let b = (double (\x:Bool. x)) true in
        | if b then a else 0
      """.stripMargin
    val ast: Exp2[Alg, Exp[TAlg]] = parser.parse(input).get

    val (ty, _, cs) = ast(Typer)(Ctx.empty(), 0)
    val solution = Unify.unify(cs)

    println("Type: " + Unify(ty, cs)(TPrint))

    val ast2 = ast(new Alg.MapSnd[Alg, Exp[TAlg]] with Impl[Exp2[Alg, Exp[TAlg]]] {
      override def mp(t: Exp[TAlg]): Exp[TAlg] = t(new TSubstImpl(solution))
    })
    go(ast2, 1)
  }

  def go(e: Exp2[Alg, Exp[TAlg]], step: Int): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(Print))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }
  }
}

