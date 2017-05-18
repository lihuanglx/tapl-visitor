package tapl.language.simplebool

import tapl.common.{Context, Exp}
import tapl.common.Util.E3

object Test {
  val parser = new Parse[Alg, TAlg] {}

  val eval = new EvalM {}

  val typer = new TyperM {}

  def main(args: Array[String]): Unit = {
    val input = "(\\f:Bool->Bool.f false) (\\x:Bool.if x then false else true)"
    val ast: E3[Alg, Exp[TAlg]] = parser.parse(input).get
    go(ast, 1)
  }

  def go(e: E3[Alg, Exp[TAlg]], step: Int): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(PrintImpl))
    println("  Type: " + e(typer)(Context.empty())(TPrintImpl))
    if (e(IsValImpl)) {
      println("Value")
    } else {
      go(e(eval), step + 1)
    }
  }
}
