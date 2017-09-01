package tapl.language.fullsimple

import tapl.common._

import scala.io.Source

object Test extends benchmark.Benchmark[Exp2[Term, Exp[Type]]] {
  val parser = new Parse[Term, Type] {}

  val name = "fullsimple"

  def main(args: Array[String]): Unit = {
    val inputFile = "examples/" + name + ".txt"
    val lines: List[String] = Source.fromFile(inputFile).getLines().toList
    lines.foreach(process)
  }

  def process(input: String): Unit = {
    println(input)
    val ast: Exp2[Term, Exp[Type]] = parser.parse(input).get
    println("Type: " + ast(Typer)(Ctx.empty())(TPrint))
    go(ast, 1)
    println("-" * 80)
  }

  def go(e: Exp2[Term, Exp[Type]], step: Int): Unit = {
    print("Step " + step.toString + ": ")
    println(e(Print))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }
  }

  def eval(e: Exp2[Term, Exp[Type]]): Exp2[Term, Exp[Type]] = if (e(IsVal)) e else eval(e(Eval))

  def benchmark(input: String): Unit = {
    val e: Exp2[Term, Exp[Type]] = parser.parse(input).get
    val t: Exp[Type] = e(Typer)(Ctx.empty())
    val _ = eval(e)
  }

  override def benchmarkParsing(i: String): Exp2[Term, Exp[Type]] =
    parser.parse(i).get

  override def benchmarkEval(e: Exp2[Term, Exp[Type]]): Exp2[Term, Exp[Type]] = {
    //val t: Exp[Type] = e(Typer)(Ctx.empty())
    eval(e)
  }
}
