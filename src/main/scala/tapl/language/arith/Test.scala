package tapl.language.arith

import tapl.language.arith.Syntax._
import tapl.language.arith.Syntax.Factory._

object Test {
  def main(args: Array[String]): Unit = {
    val a: Exp = TmIf(TmFalse(), TmZero(), TmPred(TmZero()))
    println(PrintImpl.visit(a))
  }
}
