package benchmark.parsing

import tapl.{language => visitor}
import org.scalameter._
import org.scalatest.FunSuite

import scala.io.Source

class Test extends FunSuite {

  def readLines(name: String): List[String] = {
    val inputFile = "examples/parsing/" + name + ".txt"
    Source.fromFile(inputFile).getLines().toList
  }

  def benchmark[A](inputs: List[A], process: A => Unit, rep: Int): Quantity[Double] =
    config(
      Key.exec.benchRuns -> 10
      //Key.verbose -> true
    ) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    } measure {
      1 to rep foreach { _ => inputs.foreach(process) }
    }

  def output(name: String, t1: Double, t2: Double): Unit = println(f"$name & $t1%.1f & $t2%.1f")

  def compare[A, B](name: String, modular: String => Unit, nonmod: String => Unit): Unit = {
    val lines: List[String] = readLines(name)

    val rep = 1
    val pt1 = benchmark(lines, modular, rep).value
    val pt2 = benchmark(lines, nonmod, rep).value

    output(name, pt1, pt2)
  }

  test("arith") {
    compare("arith", visitor.arith.Test.benchmarkParsing, comp.parsing.arith.Parser.input)
  }

  test("untyped") {
    compare("untyped", visitor.untyped.Test.benchmarkParsing, comp.parsing.untyped.Parser.input)
  }

  test("fulluntyped") {
    compare("fulluntyped", visitor.fulluntyped.Test.benchmarkParsing, comp.parsing.fulluntyped.Parser.input)
  }

  test("tyarith") {
    compare("tyarith", visitor.tyarith.Test.benchmarkParsing, comp.parsing.tyarith.Parser.input)
  }

  test("simplebool") {
    compare("simplebool", visitor.simplebool.Test.benchmarkParsing, comp.parsing.simplebool.Parser.input)
  }

  test("fullsimple") {
    compare("fullsimple", visitor.fullsimple.Test.benchmarkParsing, comp.parsing.fullsimple.Parser.input)
  }

  test("bot") {
    compare("bot", visitor.bot.Test.benchmarkParsing, comp.parsing.bot.Parser.input)
  }

  test("fullerror") {
    compare("fullerror", visitor.fullerror.Test.benchmarkParsing, comp.parsing.fullerror.Parser.input)
  }

  test("rcdsubbot") {
    compare("rcdsubbot", visitor.rcdsubbot.Test.benchmarkParsing, comp.parsing.rcdsubbot.Parser.input)
  }

  test("fullsub") {
    compare("fullsub", visitor.fullsub.Test.benchmarkParsing, comp.parsing.fullsub.Parser.input)
  }

  test("fullref") {
    compare("fullref", visitor.fullref.Test.benchmarkParsing, comp.parsing.fullref.Parser.input)
  }

  test("equirec") {
    compare("equirec", visitor.equirec.Test.benchmarkParsing, comp.parsing.equirec.Parser.input)
  }

  test("fullequirec") {
    compare("fullequirec", visitor.fullequirec.Test.benchmarkParsing, comp.parsing.fullequirec.Parser.input)
  }

  test("fullisorec") {
    compare("fullisorec", visitor.fullisorec.Test.benchmarkParsing, comp.parsing.fullisorec.Parser.input)
  }

  test("recon") {
    compare("recon", visitor.recon.Test.benchmarkParsing, comp.parsing.recon.Parser.input)
  }

  test("fullrecon") {
    compare("fullrecon", visitor.fullrecon.Test.benchmarkParsing, comp.parsing.fullrecon.Parser.input)
  }

  test("fullpoly") {
    compare("fullpoly", visitor.fullpoly.Test.benchmarkParsing, comp.parsing.fullpoly.Parser.input)
  }

  test("fullomega") {
    compare("fullomega", visitor.fullomega.Test.benchmarkParsing, comp.parsing.fullomega.Parser.input)
  }
}
