package benchmark

import comp.{tapl => ordinary}
import org.scalameter._
import org.scalatest.FunSuite
import tapl.{language => visitor}

import scala.io.Source

class Test extends FunSuite {

  def readLines(name: String): List[String] = {
    val inputFile = "examples/" + name + ".txt"
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

  def compare[A, B](name: String, modular: Benchmark[A], nonmod: Benchmark[B]): Unit = {
    val lines: List[String] = readLines(name)

    val parsingRep = 1
    val pt1 = benchmark(lines, modular.benchmarkParsing, parsingRep).value
    val pt2 = benchmark(lines, nonmod.benchmarkParsing, parsingRep).value

    val es1 = lines.map(modular.benchmarkParsing)
    val es2 = lines.map(nonmod.benchmarkParsing)

    val evalRep = 1
    val pe1 = benchmark(es1, modular.benchmarkEval, evalRep).value
    val pe2 = benchmark(es2, nonmod.benchmarkEval, evalRep).value

    println(f"$name & $pt1%.1f & $pt2%.1f & $pe1%.1f & $pe2%.1f")
  }

  test("arith") {
    compare("arith", visitor.arith.Test, ordinary.arith.Demo)
  }

  test("untyped") {
    compare("untyped", visitor.untyped.Test, ordinary.untyped.Demo)
  }

  test("fulluntyped") {
    compare("fulluntyped", visitor.fulluntyped.Test, ordinary.fulluntyped.Demo)
  }

  test("tyarith") {
    compare("tyarith", visitor.tyarith.Test, ordinary.tyarith.Demo)
  }

  test("simplebool") {
    compare("simplebool", visitor.simplebool.Test, ordinary.simplebool.Demo)
  }

  test("fullsimple") {
    compare("fullsimple", visitor.fullsimple.Test, ordinary.fullsimple.Demo)
  }

  test("bot") {
    compare("bot", visitor.bot.Test, ordinary.bot.Demo)
  }

  test("fullref") {
    compare("fullref", visitor.fullref.Test, ordinary.fullref.Demo)
  }

  test("fullerror") {
    compare("fullerror", visitor.fullerror.Test, ordinary.fullerror.Demo)
  }

  test("rcdsubbot") {
    compare("rcdsubbot", visitor.rcdsubbot.Test, ordinary.rcdsubbot.Demo)
  }

  test("fullsub") {
    compare("fullsub", visitor.fullsub.Test, ordinary.fullsub.Demo)
  }

  test("fullequirec") {
    compare("fullequirec", visitor.fullequirec.Test, ordinary.fullequirec.Demo)
  }

  test("fullisorec") {
    compare("fullisorec", visitor.fullisorec.Test, ordinary.fullisorec.Demo)
  }

  test("equirec") {
    compare("equirec", visitor.equirec.Test, ordinary.equirec.Demo)
  }

  test("recon") {
    compare("recon", visitor.recon.Test, ordinary.recon.Demo)
  }

  test("fullrecon") {
    compare("fullrecon", visitor.fullrecon.Test, ordinary.fullrecon.Demo)
  }

  test("fullpoly") {
    compare("fullpoly", visitor.fullpoly.Test, ordinary.fullpoly.Demo)
  }

  test("fullomega") {
    compare("fullomega", visitor.fullomega.Test, ordinary.fullomega.Demo)
  }
}
