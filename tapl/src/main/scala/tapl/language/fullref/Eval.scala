package tapl.language.fullref

import tapl.common._
import tapl.component.{ref, variant}
import tapl.language.fullsub
import tapl.language.fullref.Alg.{Query, Transform}

import scala.collection.mutable

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V]
  extends Alg[Exp2[A, V], mutable.MutableList[Exp2[A, V]] => Exp2[A, V], V]
    with IIsVal[({type lam[-X, Y] = A[X, Y, V]})#lam] with ISubst[({type lam[-X, Y] = A[X, Y, V]})#lam]
    with fullsub.Alg.Lifter[Exp2[A, V], Exp2[A, V], V, mutable.MutableList[Exp2[A, V]]]
    with variant.Alg.Lifter[Exp2[A, V], Exp2[A, V], V, mutable.MutableList[Exp2[A, V]]]
    with ref.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def go(c: mutable.MutableList[Exp2[A, V]]) =
    new fullsub.Eval[A, V] with variant.Eval[A, V] {
      override def apply(e: Exp2[A, V]): Exp2[A, V] = Eval.this.apply(e)(c)

      override def subst(m: Map[String, Exp2[A, V]]): A[Exp2[A, V], Exp2[A, V], V] = Eval.this.subst(m)

      override val isVal: A[Exp2[A, V], Boolean, V] = Eval.this.isVal
    }
}

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[mutable.MutableList[Exp2[Alg, Exp[TAlg]]] => Exp2[Alg, Exp[TAlg]]] {
  override val isVal: Alg[Exp2[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override def subst(m: Map[String, Exp2[Alg, Exp[TAlg]]]) = new SubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V]
  with fullsub.IsVal[A, V] with variant.IsVal[A, V] with ref.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V]
  with fullsub.Subst[A, V] with variant.Subst[A, V]

class SubstImpl(mp: Map[String, Exp2[Alg, Exp[TAlg]]]) extends Subst[Alg, Exp[TAlg]] with Impl[Exp2[Alg, Exp[TAlg]]] {
  override val m: Map[String, Exp2[Alg, Exp[TAlg]]] = mp
}
