import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

import gems._

package object language {

  trait BaseParser extends StandardTokenParsers with PackratParsers {
    def parse[T](parser: PackratParser[T])(input: String): T = {
      val r = phrase(parser)(new lexical.Scanner(input))
      if (r.successful) r.get else sys.error(r.toString)
    }
  }

  trait IIsVal[A[-R, E]] {
    def isVal: A[Exp[A], Boolean]
  }

  trait ISubst[A[-R, E]] {
    def subst(x: String, t: Exp[A]): A[Exp[A], Exp[A]]
  }

  trait ITEquals[A[-R, E]] {
    def tEquals: A[Exp[A], Exp[A] => Boolean]
  }

}
