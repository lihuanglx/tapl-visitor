package tapl.language.recon

import tapl.common._

object Test {
  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    //λf:Y. λa:X. f (f a)
    val input = "((\\f:Y. \\a:X. f (f a)) (\\b:Bool. if b then false else true)) true"
    val ast: Exp2[Alg, Exp[TAlg]] = parser.parse(input).get

    val (ty, _, cs) = ast(Typer)(Ctx.empty(), 0)
    val solution = Unify.unify(cs)

    println("Type: " + Unify(ty, cs)(TPrint))

    val map2 = new Alg.Map2[Alg, Exp[TAlg]] with Impl[(Exp[TAlg] => Exp[TAlg]) => Exp2[Alg, Exp[TAlg]]]

    val ast2 = ast(map2)(_ (new TSubstImpl(solution)))
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

