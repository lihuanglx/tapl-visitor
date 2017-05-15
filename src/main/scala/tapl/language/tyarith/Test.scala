package tapl.language.tyarith

import tapl.common.Exp

import scalaz.Monad
import scalaz.std.AllInstances._

object Test {
  val typer = new TyperM[Option] {
    override implicit val m: Monad[Option] = implicitly[Monad[Option]]
  }

  val eval = new EvalM {}

  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    val input = "if true then (if false then 2 else 4) else 3"
    val ast: Exp[Alg] = parser.parse(input).get
    go(ast, 1)
  }

  def go(e: Exp[Alg], step: Int): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(PrintImpl))
    println("  Type: " + e(typer).get.apply(TPrintImpl))
    if (e(IsValImpl)) {
      println("Value")
    } else {
      go(e(eval), step + 1)
    }
  }
}