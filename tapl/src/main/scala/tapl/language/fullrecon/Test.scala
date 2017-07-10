package tapl.language.fullrecon

import tapl.common._

import scala.io.Source

object Test {
  val parser = new Parse[Alg, TAlg] {}

  val name = "fullrecon"

  def main(args: Array[String]): Unit = {
    val inputFile = "examples/" + name + ".txt"
    val lines: List[String] = Source.fromFile(inputFile).getLines().toList
    lines.foreach(process)
  }

  def process(input: String): Unit = {
    println(input)
    val ast: Exp2[Alg, Exp[TAlg]] = parser.parse(input).get
    val (ty, _, cs) = ast(Typer)(Ctx.empty(), 0)
    val solution = Unify.unify(cs)
    println("Type: " + Unify(ty, cs)(TPrint))
    val map2 = new Alg.Map2[Alg, Exp[TAlg]] with Impl[(Exp[TAlg] => Exp[TAlg]) => Exp2[Alg, Exp[TAlg]]]
    val ast2 = ast(map2)(_ (new TSubstImpl(solution)))
    go(ast2, 1)
    println("-" * 80)
  }

  def go(e: Exp2[Alg, Exp[TAlg]], step: Int): Unit = {
    print("Step " + step.toString + ": ")
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
    val (ty, _, cs) = e(Typer)(Ctx.empty(), 0)
    val t: Exp[TAlg] = Unify(ty, cs)
    val _ = eval(e)
  }
}

