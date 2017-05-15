package tapl.language.untyped

import tapl.common.Exp

object Test {
  val eval = new EvalM {}

  val parser = new Parse[Alg] {}

  def main(args: Array[String]): Unit = {
    val input = "(\\x.(\\x.x)) (\\y.y)"
    val ast: Exp[Alg] = parser.parse(input).get
    go(ast, 1)
  }

  def go(e: Exp[Alg], step: Int): Unit = {
    print("Step " ++ step.toString ++ ": ")
    println(e(PrintImpl))
    if (e(IsValImpl)) {
      println("Value")
    } else {
      go(e(eval), step + 1)
    }
  }

}
