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



}
