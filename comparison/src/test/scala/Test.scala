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

  test("arith") {
    val lines = readLines("arith")

    val time = benchmark(lines, ordinary.arith.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.arith.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("untyped") {
    val lines = readLines("untyped")

    val time = benchmark(lines, ordinary.untyped.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.untyped.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("fulluntyped") {
    val lines = readLines("fulluntyped")

    val time = benchmark(lines, ordinary.fulluntyped.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.fulluntyped.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("tyarith") {
    val lines = readLines("tyarith")

    val time = benchmark(lines, ordinary.tyarith.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.tyarith.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("simplebool") {
    val lines = readLines("simplebool")

    val time = benchmark(lines, ordinary.simplebool.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.simplebool.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("fullsimple") {
    val lines = readLines("fullsimple")

    val time = benchmark(lines, ordinary.fullsimple.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.fullsimple.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("bot") {
    val lines = readLines("bot")

    val time = benchmark(lines, ordinary.bot.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.bot.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("fullref") {
    val lines = readLines("fullref")

    val time = benchmark(lines, ordinary.fullref.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.fullref.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("fullerror") {
    val lines = readLines("fullerror")

    val time = benchmark(lines, ordinary.fullerror.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.fullerror.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("rcdsubbot") {
    val lines = readLines("rcdsubbot")

    val time = benchmark(lines, ordinary.rcdsubbot.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.rcdsubbot.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("fullsub") {
    val lines = readLines("fullsub")

    val time = benchmark(lines, ordinary.fullsub.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.fullsub.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("fullequirec") {
    val lines = readLines("fullequirec")

    val time = benchmark(lines, ordinary.fullequirec.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.fullequirec.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("fullisorec") {
    val lines = readLines("fullisorec")

    val time = benchmark(lines, ordinary.fullisorec.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.fullisorec.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("equirec") {
    val lines = readLines("equirec")

    val time = benchmark(lines, ordinary.equirec.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.equirec.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("recon") {
    val lines = readLines("recon")

    val time = benchmark(lines, ordinary.recon.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.recon.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("fullrecon") {
    val lines = readLines("fullrecon")

    val time = benchmark(lines, ordinary.fullrecon.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.fullrecon.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("fullpoly") {
    val lines = readLines("fullpoly")

    val time = benchmark(lines, ordinary.fullpoly.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.fullpoly.Test.benchmark)
    println(s"Visitor time: $t2")
  }

  test("fullomega") {
    val lines = readLines("fullomega")

    val time = benchmark(lines, ordinary.fullomega.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.fullomega.Test.benchmark)
    println(s"Visitor time: $t2")
  }
}
