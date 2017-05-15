package tapl.language.bot

import tapl.common.Exp
import tapl.common.Util.E3

object Test {
  val parser = new Parse[Alg, TAlg] {}

  val eval = new EvalM {}

  def main(args: Array[String]): Unit = {
    val input = "(\\x:Top.x) y"
    val ast: E3[Alg, Exp[TAlg]] = parser.parse(input).get
    go(ast, 1)
  }

  def go(e: E3[Alg, Exp[TAlg]], step: Int): Unit = {
    print("Step " ++ step.toString ++ ": ")
    println(e(PrintImpl))
    if (e(IsValImpl)) {
      println("Value")
    } else {
      go(e(eval), step + 1)
    }
  }

}
