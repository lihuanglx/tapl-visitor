package tapl.language.fulluntyped

import tapl.common._

import scala.io.Source

object Test extends benchmark.Benchmark[Exp[Term]] {
  val parser = new Parse[Term] {}

  val name = "fulluntyped"

  def main(args: Array[String]): Unit = {
    val inputFile = "examples/" + name + ".txt"
    val lines: List[String] = Source.fromFile(inputFile).getLines().toList
    lines.foreach(process)
  }

  def process(input: String): Unit = {
    println(input)
    val ast: Exp[Term] = parser.parse(input).get
    go(ast, 1)
    println("-" * 80)
  }

  def go(e: Exp[Term], step: Int): Unit = {
    print("Step " ++ step.toString ++ ": ")
    println(e(Print))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }
  }

  def eval(e: Exp[Term]): Exp[Term] = if (e(IsVal)) e else eval(e(Eval))

  def benchmark(input: String): Unit = {
    val e: Exp[Term] = parser.parse(input).get
    val _ = eval(e)
  }

  override def benchmarkParsing(i: String): Exp[Term] = parser.parse(i).get

  override def benchmarkEval(e: Exp[Term]): Exp[Term] = eval(e)

}
