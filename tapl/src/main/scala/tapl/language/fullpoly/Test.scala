package tapl.language.fullpoly

import tapl.common._

import scala.io.Source

object Test {
  val parser = new Parse[Alg, TAlg] {}

  val name = "fullpoly"

  def main(args: Array[String]): Unit = {
    val inputFile = "examples/" + name + ".txt"
    val lines: List[String] = Source.fromFile(inputFile).getLines().toList
    lines.foreach(process)
  }

  def process(input: String): Unit = {
    println(input)
    val ast: Exp2[Alg, Exp[TAlg]] = parser.parse(input).get
    println("Type: " + ast(Typer)(Ctx.empty())(TPrint))
    go(ast, 1)
    println("-" * 80)
  }

  def go(e: Exp2[Alg, Exp[TAlg]], step: Int): Unit = {
    print(s"Step $step: ")
    println(e(Print))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }
  }

  def eval(e: Exp2[Alg, Exp[TAlg]]): Exp2[Alg, Exp[TAlg]] = if (e(IsVal)) e else eval(e(Eval))

  def benchmark(input: String): Unit = {
    val e: Exp2[Alg, Exp[TAlg]] = parser.parse(input).get
    val t: Exp[TAlg] = e(Typer)(Ctx.empty())
    val _ = eval(e)
  }
}
