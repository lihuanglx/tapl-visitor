package tapl.language.arith

import tapl.common.Exp


object Test {
  val parser = new Parse[Alg] {}

  def main(args: Array[String]): Unit = {
    val input = "if true then (if false then 2 else (pred 4)) else 3"
    val ast: Exp[Alg] = parser.parse(input).get
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
