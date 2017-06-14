package tapl.language.fullref

import tapl.common._

import scala.collection.mutable
import scala.collection.mutable.MutableList

object Test {
  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    val input = "(\\r:(Ref Bool). if !r then 1 else {r := false; 2}) (ref false)"
    val ast: TExp[Alg, Exp[TAlg]] = parser.parse(input).get
    println("Type: " + ast(Typer)(Ctx.empty())(Ctx.empty())(TPrint))
    go(ast, 1, mutable.MutableList())
  }

  def go(e: TExp[Alg, Exp[TAlg]], step: Int, c: mutable.MutableList[TExp[Alg, Exp[TAlg]]]): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(Print))

    if (e(IsVal)) {
      println("Value")
    } else {
      val nxt = e(Eval)(c)
      println(c)
      go(nxt, step + 1, c)
    }
  }
}
