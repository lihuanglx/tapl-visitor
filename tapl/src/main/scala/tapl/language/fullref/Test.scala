package tapl.language.fullref

import tapl.common._

import scala.collection.mutable
import scala.io.Source

object Test {
  val parser = new Parse[Alg, TAlg] {}

  val name = "fullref"

  def main(args: Array[String]): Unit = {
    val inputFile = "examples/" + name + ".txt"
    val lines: List[String] = Source.fromFile(inputFile).getLines().toList
    lines.foreach(process)
  }

  def process(input: String): Unit = {
    println(input)
    val ast: Exp2[Alg, Exp[TAlg]] = parser.parse(input).get
    println("Type: " + ast(Typer)(Ctx.empty())(Ctx.empty())(TPrint))
    go(ast, 1, mutable.MutableList())
    println("-" * 80)
  }

  def go(e: Exp2[Alg, Exp[TAlg]], step: Int, c: mutable.MutableList[Exp2[Alg, Exp[TAlg]]]): Unit = {
    println(s"Step $step, context = $c: ")
    println(e(Print))
    if (e(IsVal)) {
      println("Value")
    } else {
      val nxt = e(Eval)(c)
      go(nxt, step + 1, c)
    }
  }
}
