package tapl.language.arith

import tapl.language.arith.Syntax.Factory._
import tapl.language.arith.Syntax._

import scalaz.Monad
import scalaz.std.AllInstances._


object Test {

  def main(args: Array[String]): Unit = {

    val exp: Exp = TmIf(TmFalse(), TmZero(), TmPred(TmZero()))

    println(PrintImpl.visit(exp))

    val eval = new EvalM[Option] {
      override implicit val m: Monad[Option] = implicitly[Monad[Option]]
    }

    println(eval.visit(exp))
  }

}
