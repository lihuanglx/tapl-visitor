package tapl.language.arith

import tapl.common.Exp

import scalaz.Monad
import scalaz.std.AllInstances._


object Test {

  def main(args: Array[String]): Unit = {
    val f = Factory

    val exp: Exp[Alg] = f.TmIf(f.TmFalse(), f.TmZero(), f.TmSucc(f.TmZero()))

    println(PrintImpl.apply(exp))

    val eval = new EvalM[Option] {
      override implicit val m: Monad[Option] = implicitly[Monad[Option]]
    }

    println(eval.apply(exp))

    val input = "if true then 10 else 3"

    val parser = new Parse[Alg] {
      override val f: Factory[Alg] = Factory
    }

    val ast: Exp[Alg] = parser.parse(input) match {
      case Some(t) => t
      case None => sys.error("error")
    }

    println(ast(eval))

  }

}
