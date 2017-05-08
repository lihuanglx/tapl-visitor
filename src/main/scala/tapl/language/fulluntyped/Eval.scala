package tapl.language.fulluntyped

import tapl.common.Exp
import tapl.component.{floatstring, let, record}
import tapl.language.{arith, untyped}

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with arith.Eval[A, M] with untyped.Eval[A, M]
  with floatstring.Eval[A, M] with let.Eval[A, M] with record.Eval[A, M]

trait EvalM[M[_]] extends Eval[Alg, M] with Impl[M[Exp[Alg]]] {
  override val isVal: Alg[Exp[Alg], Boolean] = IsValImpl

  override val isFuncVal: Alg[Exp[Alg], Option[(String, Exp[Alg])]] = IsFuncValImpl

  override val subst: (String, Exp[Alg]) => Alg[Exp[Alg], Exp[Alg]] = (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with arith.IsVal[A] with untyped.IsVal[A]
  with floatstring.IsVal[A] with record.IsVal[A]

object IsValImpl extends IsVal[Alg] with Impl[Boolean]

trait IsFuncVal[A[-R, _]] extends Query[Exp[A], Option[(String, Exp[A])]] with untyped.IsFuncVal[A]

object IsFuncValImpl extends IsFuncVal[Alg] with Impl[Option[(String, Exp[Alg])]]

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with untyped.Subst[A]

class SubstImpl(_x: String, _e: Exp[Alg]) extends Subst[Alg] with Impl[Exp[Alg]] {
  override val x: String = _x
  override val e: Exp[Alg] = _e
}
