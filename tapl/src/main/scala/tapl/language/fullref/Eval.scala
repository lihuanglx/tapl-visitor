package tapl.language.fullref

import tapl.common._
import tapl.component.{ref, variant}
import tapl.language.fullsub
import tapl.language.fullref.Alg.{Query, Transform}

import scala.collection.mutable

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V]
  extends Alg[TExp[A, V], mutable.MutableList[TExp[A, V]] => TExp[A, V], V]
    with IIsVal[({type lam[-X, Y] = A[X, Y, V]})#lam] with ISubst[({type lam[-X, Y] = A[X, Y, V]})#lam]
    with fullsub.Alg.Lifter[TExp[A, V], TExp[A, V], V, mutable.MutableList[TExp[A, V]]]
    with variant.Alg.Lifter[TExp[A, V], TExp[A, V], V, mutable.MutableList[TExp[A, V]]]
    with ref.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def go(c: mutable.MutableList[TExp[A, V]]) =
    new fullsub.Eval[A, V] with variant.Eval[A, V] {
      override def apply(e: TExp[A, V]): TExp[A, V] = Eval.this.apply(e)(c)

      override val subst: (String, TExp[A, V]) => A[TExp[A, V], TExp[A, V], V] = Eval.this.subst

      override val isVal: A[TExp[A, V], Boolean, V] = Eval.this.isVal
    }
}

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[mutable.MutableList[TExp[Alg, Exp[TAlg]]] => TExp[Alg, Exp[TAlg]]] {
  override val isVal: Alg[TExp[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override val subst: (String, TExp[Alg, Exp[TAlg]]) => Alg[TExp[Alg, Exp[TAlg]], TExp[Alg, Exp[TAlg]], Exp[TAlg]] =
    (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V]
  with fullsub.IsVal[A, V] with variant.IsVal[A, V] with ref.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V]
  with fullsub.Subst[A, V] with variant.Subst[A, V]

class SubstImpl(_x: String, _e: TExp[Alg, Exp[TAlg]]) extends Subst[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val x: String = _x
  override val e: TExp[Alg, Exp[TAlg]] = _e
}

