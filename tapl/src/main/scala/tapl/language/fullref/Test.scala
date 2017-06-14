package tapl.language.fullref

import tapl.common._

object Test {
  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    val input = "\\r:(Ref Bool). if !r then 1 else 2"
    val ast: TExp[Alg, Exp[TAlg]] = parser.parse(input).get
    go(ast, 1)
  }

  def go(e: TExp[Alg, Exp[TAlg]], step: Int): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(Print))
    println("  Type: " + e(Typer)(Ctx.empty())(Ctx.empty())(TPrint))
    /*
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }*/
  }
}
