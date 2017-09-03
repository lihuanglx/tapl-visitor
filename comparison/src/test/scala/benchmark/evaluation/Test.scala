package benchmark.evaluation

import benchmark.Benchmark
import tapl.{language => visitor}
import org.scalameter._
import org.scalatest.FunSuite

import scala.io.Source

class Test extends FunSuite {

  def readLines(name: String): List[String] = {
    val inputFile = "examples/evaluation/" + name + ".txt"
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

  def compare[A, B](name: String, modular: Benchmark[A], nonmod: Benchmark[B]): Unit = {
    val lines: List[String] = readLines(name)

    val es1 = lines.map(modular.benchmarkParsing)
    val es2 = lines.map(nonmod.benchmarkParsing)

    val rep = 1
    val pe1 = benchmark(es1, modular.benchmarkEval, rep).value
    val pe2 = benchmark(es2, nonmod.benchmarkEval, rep).value

    output(name, pe1, pe2)
  }

  test("arith") {
    compare("arith", visitor.arith.Test, comp.tapl.arith.Demo)
  }

  test("untyped") {
    compare("untyped", visitor.untyped.Test, comp.tapl.untyped.Demo)
  }

  test("fulluntyped") {
    compare("fulluntyped", visitor.fulluntyped.Test, comp.tapl.fulluntyped.Demo)
  }

  test("tyarith") {
    compare("tyarith", visitor.tyarith.Test, comp.tapl.tyarith.Demo)
  }

  test("simplebool") {
    compare("simplebool", visitor.simplebool.Test, comp.tapl.simplebool.Demo)
  }

  test("fullsimple") {
    compare("fullsimple", visitor.fullsimple.Test, comp.tapl.fullsimple.Demo)
  }

  test("bot") {
    compare("bot", visitor.bot.Test, comp.tapl.bot.Demo)
  }

  test("fullerror") {
    compare("fullerror", visitor.fullerror.Test, comp.tapl.fullerror.Demo)
  }

  test("rcdsubbot") {
    compare("rcdsubbot", visitor.rcdsubbot.Test, comp.tapl.rcdsubbot.Demo)
  }

  test("fullsub") {
    compare("fullsub", visitor.fullsub.Test, comp.tapl.fullsub.Demo)
  }

  test("fullref") {
    compare("fullref", visitor.fullref.Test, comp.tapl.fullref.Demo)
  }

  test("equirec") {
    compare("equirec", visitor.equirec.Test, comp.tapl.equirec.Demo)
  }

  test("fullequirec") {
    compare("fullequirec", visitor.fullequirec.Test, comp.tapl.fullequirec.Demo)
  }

  test("fullisorec") {
    compare("fullisorec", visitor.fullisorec.Test, comp.tapl.fullisorec.Demo)
  }

  test("recon") {
    compare("recon", visitor.recon.Test, comp.tapl.recon.Demo)
  }

  test("fullrecon") {
    compare("fullrecon", visitor.fullrecon.Test, comp.tapl.fullrecon.Demo)
  }

  test("fullpoly") {
    compare("fullpoly", visitor.fullpoly.Test, comp.tapl.fullpoly.Demo)
  }

  test("fullomega") {
    compare("fullomega", visitor.fullomega.Test, comp.tapl.fullomega.Demo)
  }

}
