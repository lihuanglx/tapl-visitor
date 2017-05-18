package tapl.language.fulluntyped

import tapl.common.Exp

object Test {
  val parser = new Parse[Alg] {}

  def main(args: Array[String]): Unit = {
    //val input = "{x=\\x.x, y=(\\x.x)(\\x.x)}.x"
    val input = "(\\x.\\x.x) 3"
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
