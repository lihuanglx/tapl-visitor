package comp.tapl.fulluntyped

import scala.io.Source

object Demo {

  import Evaluator._
  import comp.util.Print._
  import PrettyPrinter._

  val width = 60

  val name = "untyped"

  def main(args: Array[String]): Unit = {
    val inputFile = "examples/" + name + ".txt"
    val lines: List[String] = Source.fromFile(inputFile).getLines().toList
    lines.foreach(process)
  }

  def process(i: String): Unit = {
    println(i)
    val e: Term = Parser.input(i)(Context())
    val doc1 = g2(ptmATerm(true, Context(), e))
    println(print(doc1, width))
    val t2 = eval(Context(), e)
    val doc2 = g2(ptmATerm(true, Context(), t2))
    println(print(doc2, width))
    println("-" * 80)
  }

  // process without output
  def benchmark(i: String): Unit = {
    val e = Parser.input(i)(Context())
    val _ = eval(Context(), e)
  }

}
