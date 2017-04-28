package tapl.language.untyped

import tapl.common.Exp
import tapl.component._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends
  Alg[Exp[A], M[Exp[A]]] with lambda.Eval[A, M] with varapp.Eval[A, M]

trait EvalM[M[_]] extends Eval[Alg, M] {
  override def apply(e: Exp[Alg]): M[Exp[Alg]] = e(this)

  override val f: Alg[Exp[Alg], Exp[Alg]] = Factory

  override val isVal: Alg[Exp[Alg], Boolean] = IsValImpl

  override val isFuncVal: Alg[Exp[Alg], Option[(String, Exp[Alg])]] = IsFuncValImpl

  override val subst: (String, Exp[Alg]) => Alg[Exp[Alg], Exp[Alg]] = (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with lambda.IsVal[A] with varapp.IsVal[A]

object IsValImpl extends IsVal[Alg] with Impl[Boolean]

trait IsFuncVal[A[-R, _]] extends Query[Exp[A], Option[(String, Exp[A])]] with lambda.IsFuncVal[A]

object IsFuncValImpl extends IsFuncVal[Alg] with Impl[Option[(String, Exp[Alg])]]

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with varapp.Subst[A]

class SubstImpl(_x: String, _e: Exp[Alg]) extends Subst[Alg] with Impl[Exp[Alg]] {
  override val x: String = _x
  override val e: Exp[Alg] = _e
  override val f: Factory[Alg] = Factory
}