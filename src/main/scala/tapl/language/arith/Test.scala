package tapl.language.arith

import tapl.common.Exp
import tapl.language.arith.Syntax.Factory._
import tapl.language.arith.Syntax._

import scalaz.Monad
import scalaz.std.AllInstances._


object Test {

  def main(args: Array[String]): Unit = {

    val exp: Exp[Alg] = TmIf(TmFalse(), TmZero(), TmPred(TmZero()))

    println(PrintImpl.visit(exp))

    val eval = new EvalM[Option] {
      override implicit val m: Monad[Option] = implicitly[Monad[Option]]
    }

    println(eval.visit(exp))

    val input = "if true then 10 else 3"

    val parser = new Parse[Alg] {
      override val f: Factory[Alg] = new Factory[Alg] {}
    }

    val ast: Exp[Alg] = parser.parse(input) match {
      case Some(t) => t
      case None => sys.error("error")
    }

    println(ast(eval))

  }

}
