package tapl.language.arith

import tapl.common.Exp

import scalaz.Monad
import scalaz.std.AllInstances._


object Test {

  val f = Factory

  val eval = new EvalM[Option] {
    override implicit val m: Monad[Option] = implicitly[Monad[Option]]

    override lazy val f: Alg[Exp[Alg], Exp[Alg]] = ???
  }

  val parser = new Parse[Alg] {}

  def main(args: Array[String]): Unit = {
    val input = "if true then (if false then 2 else 4) else 3"
    val ast: Exp[Alg] = parser.parse(input).get
    go(ast, 1)
  }

  def go(e: Exp[Alg], step: Int): Unit = {
    print("Step " ++ step.toString ++ ": ")
    println(e(PrintImpl))
    if (e(IsValImpl)) {
      println("Value")
    } else {
      go(e(eval).get, step + 1)
    }
  }

}
