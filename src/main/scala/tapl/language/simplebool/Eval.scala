package tapl.language.simplebool

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.{typed, typedbool}

trait Eval[A[-R, E, -F] <: Alg[R, E, F], M[_], V] extends Alg[E3[A, V], M[E3[A, V]], V]
  with typed.Eval[A, M, V] with typedbool.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam, M]

trait EvalM[M[_]] extends Eval[Alg, M, Exp[TAlg]] with Impl[M[E3[Alg, Exp[TAlg]]]] {
  override val isVal: Alg[E3[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsValImpl

  override val isFuncVal: Alg[E3[Alg, Exp[TAlg]], Option[(String, E3[Alg, Exp[TAlg]])], Exp[TAlg]] = IsFuncValImpl

  override val subst: (String, E3[Alg, Exp[TAlg]]) => Alg[E3[Alg, Exp[TAlg]], E3[Alg, Exp[TAlg]], Exp[TAlg]] =
    (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, E, -F], V] extends Query[E3[A, V], Boolean, V]
  with typed.IsVal[A, V] with typedbool.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]

object IsValImpl extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait IsFuncVal[A[-R, E, -F], V] extends Query[E3[A, V], Option[(String, E3[A, V])], V] with typed.IsFuncVal[A, V]

object IsFuncValImpl extends IsFuncVal[Alg, Exp[TAlg]] with Impl[Option[(String, E3[Alg, Exp[TAlg]])]]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with typed.Subst[A, V]

class SubstImpl(_x: String, _e: E3[Alg, Exp[TAlg]]) extends Subst[Alg, Exp[TAlg]] with Impl[E3[Alg, Exp[TAlg]]] {
  override val x: String = _x
  override val e: E3[Alg, Exp[TAlg]] = _e
}
