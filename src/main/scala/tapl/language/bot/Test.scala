package tapl.language.bot

import tapl.common.Exp
import tapl.common.Util.E3

object Test {
  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    val input = "\\x:Top.x"
    val ast: E3[Alg, Exp[TAlg]] = parser.parse(input).get
    println(ast(PrintImpl))
  }
}
