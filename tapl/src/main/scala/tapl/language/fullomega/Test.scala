package tapl.language.fullomega

import tapl.common._

import scala.collection.mutable
import scala.io.Source

object Test extends benchmark.Benchmark[Exp3[Term, Exp2[Type, Exp[Kind]], Exp[Kind]]] {
  val parser = new Parse[Term, Type, Kind] {}

  type E = Exp3[Term, Exp2[Type, Exp[Kind]], Exp[Kind]]

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

  def eval(e: E, c: mutable.MutableList[E]): E =
    if (e(IsVal)) e
    else {
      val nxt = e(Eval)(c)
      eval(nxt, c)
    }

  def benchmark(input: String): Unit = {
    val e: E = parser.parse(input).get
    val t = e(Typer)(Ctx.empty())
    val _ = eval(e, mutable.MutableList())
  }

  override def benchmarkParsing(i: String): Exp3[Term, Exp2[Type, Exp[Kind]], Exp[Kind]] =
    parser.parse(i).get

  override def benchmarkEval(e: Exp3[Term, Exp2[Type, Exp[Kind]], Exp[Kind]]): Exp3[Term, Exp2[Type, Exp[Kind]], Exp[Kind]] = {
    //val t = e(Typer)(Ctx.empty())
    eval(e, mutable.MutableList())
  }
}
