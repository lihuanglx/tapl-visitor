import org.scalatest.FunSuite
import org.scalameter._
import comp.{tapl => ordinary}
import tapl.{language => visitor}

import scala.io.Source

class Test extends FunSuite {

  def readLines(name: String): List[String] = {
    val inputFile = "examples/" + name + ".txt"
    Source.fromFile(inputFile).getLines().toList
  }

  def benchmark(inputs: List[String], process: String => Unit, rep: Int = 200): Quantity[Double] =
    config(
      Key.exec.benchRuns -> 20
      //Key.verbose -> true
    ) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    } measure {
      1 to rep foreach { _ => inputs.foreach(process) }
    }

  def output(name: String, t1: Double, t2: Double): Unit = println(f"$name & $t1%.1f & $t2%.1f")

  test("arith") {
    val name = "arith"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.arith.Demo.benchmark)
    val t2 = benchmark(lines, visitor.arith.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("untyped") {
    val name = "untyped"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.untyped.Demo.benchmark)
    val t2 = benchmark(lines, visitor.untyped.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("fulluntyped") {
    val name = "fulluntyped"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.fulluntyped.Demo.benchmark)
    val t2 = benchmark(lines, visitor.fulluntyped.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("tyarith") {
    val name = "tyarith"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.tyarith.Demo.benchmark)
    val t2 = benchmark(lines, visitor.tyarith.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("simplebool") {
    val name = "simplebool"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.simplebool.Demo.benchmark)
    val t2 = benchmark(lines, visitor.simplebool.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("fullsimple") {
    val name = "fullsimple"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.fullsimple.Demo.benchmark)
    val t2 = benchmark(lines, visitor.fullsimple.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("bot") {
    val name = "bot"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.bot.Demo.benchmark)
    val t2 = benchmark(lines, visitor.bot.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("fullref") {
    val name = "fullref"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.fullref.Demo.benchmark)
    val t2 = benchmark(lines, visitor.fullref.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("fullerror") {
    val name = "fullerror"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.fullerror.Demo.benchmark)
    val t2 = benchmark(lines, visitor.fullerror.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("rcdsubbot") {
    val name = "rcdsubbot"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.rcdsubbot.Demo.benchmark)
    val t2 = benchmark(lines, visitor.rcdsubbot.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("fullsub") {
    val name = "fullsub"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.fullsub.Demo.benchmark)
    val t2 = benchmark(lines, visitor.fullsub.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("fullequirec") {
    val name = "fullequirec"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.fullequirec.Demo.benchmark)
    val t2 = benchmark(lines, visitor.fullequirec.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("fullisorec") {
    val name = "fullisorec"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.fullisorec.Demo.benchmark)
    val t2 = benchmark(lines, visitor.fullisorec.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("equirec") {
    val name = "equirec"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.equirec.Demo.benchmark)
    val t2 = benchmark(lines, visitor.equirec.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("recon") {
    val name = "recon"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.recon.Demo.benchmark)
    val t2 = benchmark(lines, visitor.recon.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("fullrecon") {
    val name = "fullrecon"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.fullrecon.Demo.benchmark)
    val t2 = benchmark(lines, visitor.fullrecon.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("fullpoly") {
    val name = "fullpoly"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.fullpoly.Demo.benchmark)
    val t2 = benchmark(lines, visitor.fullpoly.Test.benchmark)

    output(name, t1.value, t2.value)
  }

  test("fullomega") {
    val name = "fullomega"
    val lines = readLines(name)

    val t1 = benchmark(lines, ordinary.fullomega.Demo.benchmark)
    val t2 = benchmark(lines, visitor.fullomega.Test.benchmark)

    output(name, t1.value, t2.value)
  }
}
