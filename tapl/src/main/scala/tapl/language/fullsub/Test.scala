package tapl.language.fullsub

import tapl.common._

object Test {
  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    val input = "(\\r: {x: Nat}. r.x) if true then {x = 3, y = true} else {x = 5, y = false, z = 1}"
    val ast: Exp2[Alg, Exp[TAlg]] = parser.parse(input).get
    go(ast, 1)
  }

  def go(e: Exp2[Alg, Exp[TAlg]], step: Int): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(Print))
    println("  Type: " + e(Typer)(Ctx.empty())(TPrint))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }
  }
}
