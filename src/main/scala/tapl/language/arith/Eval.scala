package tapl.language.arith

import tapl.common.Exp
import tapl.component._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]]
  with bool.Eval[A, M] with nat.Eval[A, M] {

  override def matcher[E]: Matcher[A, E]
}


trait EvalM[M[_]] extends Eval[Alg, M] {
  override def apply(e: Exp[Alg]): M[Exp[Alg]] = e(this)

  override val f = Factory

  override val isVal: Alg[Exp[Alg], Boolean] = IsValImpl

  override def matcher[E]: Matcher[Alg, E] = new MatcherImpl[E] {}
}


trait IsVal[A[-R, _]] extends Alg[Exp[A], Boolean] with bool.IsVal[A] with nat.IsVal[A]

object IsValImpl extends IsVal[Alg] with Impl[Boolean]
