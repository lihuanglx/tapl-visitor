package tapl.language.arith

import tapl.common.Exp
import tapl.component._

trait Matcher[A[-X, Y], E] extends Alg[Exp[A], E] with bool.Matcher[A, E] with nat.Matcher[A, E]

trait MatcherImpl[E] extends Matcher[Alg, E] {
  override def apply(e: Exp[Alg]): E = e(this)
}
