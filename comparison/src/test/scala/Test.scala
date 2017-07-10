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

  test("bot") {
    val lines = readLines("bot")

    val time = benchmark(lines, ordinary.bot.Demo.benchmark)
    println(s"Time: $time")

    val t2 = benchmark(lines, visitor.bot.Test.benchmark)
    println(s"Visitor time: $t2")
  }

}
