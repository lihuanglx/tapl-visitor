package tapl.language.equirec

import tapl.common._

object Test {
  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    //val input = "\\f:(Rec X.X->A)->T. (\\x:(((Rec X.X->A)->A)->A)->A. f x)"
    val input = "\\f:((((Rec X.X->A)->A)->A)->A)->T. (\\x:(Rec X.X->A). f x)"
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
