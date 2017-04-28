package tapl.language.arith

import tapl.common.Exp
import tapl.component._

trait Print[A[-R, _]] extends Alg[Exp[A], String] with bool.Print[A] with nat.Print[A]

object PrintImpl extends Print[Alg] with Impl[String] {
  override def matcher[E]: Matcher[Alg, E] = new MatcherImpl[E] {}
}