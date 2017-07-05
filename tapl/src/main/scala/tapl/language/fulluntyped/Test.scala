package tapl.language.fulluntyped

import tapl.common._

import scala.io.Source

object Test {
  val parser = new Parse[Alg] {}

  val name = "fulluntyped"

  def main(args: Array[String]): Unit = {
    val inputFile = "examples/" + name + ".txt"
    val lines: List[String] = Source.fromFile(inputFile).getLines().toList
    lines.foreach(process)
  }

  def process(input: String): Unit = {
    println(input)
    val ast: Exp[Alg] = parser.parse(input).get
    go(ast, 1)
    println("-" * 80)
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
