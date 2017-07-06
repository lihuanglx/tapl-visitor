package tapl.language.fullomega

import tapl.common._

import scala.collection.mutable
import scala.io.Source

object Test {
  val parser = new Parse[Alg, TAlg, KAlg] {}

  type E = Exp3[Alg, Exp2[TAlg, Exp[KAlg]], Exp[KAlg]]

  val name = "fullomega"

  def main(args: Array[String]): Unit = {
    val inputFile = "examples/" + name + ".txt"
    val lines: List[String] = Source.fromFile(inputFile).getLines().toList
    lines.foreach(process)
  }

  def process(input: String): Unit = {
    println(input)
    val ast: E = parser.parse(input).get
    println("Type: " + ast(Typer)(Ctx.empty())(Ctx.empty())(Ctx.empty())(TPrint))
    go(ast, mutable.MutableList(), 1)
    println("-" * 80)
  }

  def go(e: E, c: mutable.MutableList[E], step: Int): Unit = {
    print(s"Step $step: ")
    println(e(Print))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval)(c), c, step + 1)
    }
  }
}
