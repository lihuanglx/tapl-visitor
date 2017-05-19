package tapl.language.rcdsubbot

import tapl.common.{Context, Exp}
import tapl.common.Util.E3

object Test {
  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    val input = "(\\r:{x:Top, y:Top}. r.x) {x = (\\x:Top.x), y = (\\y:Bot.y)}"
    val ast: E3[Alg, Exp[TAlg]] = parser.parse(input).get
    go(ast, 1)
  }

  def go(e: E3[Alg, Exp[TAlg]], step: Int): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(Print))
    println("  Type: " + e(Typer)(Context.empty())(TPrint))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }
  }
}
