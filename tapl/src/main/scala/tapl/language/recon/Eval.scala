package tapl.language.recon

import tapl.common._
import tapl.component.typed
import tapl.language.tyarith

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with typed.Eval[A, V] with tyarith.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[E3[Alg, Exp[TAlg]]] {
  override val isVal: Alg[E3[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override val subst: (String, E3[Alg, Exp[TAlg]]) => Alg[E3[Alg, Exp[TAlg]], E3[Alg, Exp[TAlg]], Exp[TAlg]] =
    (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, E, -F], V] extends Query[E3[A, V], Boolean, V] with typed.IsVal[A, V]
  with tyarith.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with typed.Subst[A, V]

class SubstImpl(_x: String, _e: E3[Alg, Exp[TAlg]]) extends Subst[Alg, Exp[TAlg]] with Impl[E3[Alg, Exp[TAlg]]] {
  override val x: String = _x
  override val e: E3[Alg, Exp[TAlg]] = _e
}
