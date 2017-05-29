package tapl.language.untyped

import tapl.common._

object Test {
  def main(args: Array[String]): Unit = {
    val input = "(\\x.(\\x.x)) (\\y.y)"
    val ast: Exp[Alg] = Parse.parse(input).get
    go(ast, 1)
  }

  def go(e: Exp[Alg], step: Int): Unit = {
    print("Step " ++ step.toString ++ ": ")
    println(e(Print))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }
  }

}
