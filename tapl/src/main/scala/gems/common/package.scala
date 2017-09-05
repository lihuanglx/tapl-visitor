package gems

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

package object common {
  trait SExp[-A[-R, _], -B[-F, _]] {
    def apply[E](alg: A[SExp[B, B], E]): E
  }

  type Exp[-A[-R, _]] = SExp[A, A]

  type SExp2[-A[-R, E, -F], -B[-R, E], +V] = SExp[A[-?, ?, V], B[-?, ?]]

  type Exp2[-A[-R, E, -F], +V] = SExp2[A, A[-?, ?, V], V]

  trait Default[T] {
    def default: T
  }

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
}
