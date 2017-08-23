package tapl.language.fullref

import tapl.common._
import tapl.component.{ref, variant}
import tapl.language.fullsub
import tapl.language.fullref.Term.{Query, Transform}

import scala.collection.mutable

trait Eval[A[-R, E, -F] <: Term[R, E, F], V]
  extends Term[Exp2[A, V], mutable.MutableList[Exp2[A, V]] => Exp2[A, V], V]
    with IIsVal[A[-?, ?, V]] with ISubst[A[-?, ?, V]]
    with fullsub.Term.Lifter[Exp2[A, V], Exp2[A, V], V, mutable.MutableList[Exp2[A, V]]]
    with variant.Term.Lifter[Exp2[A, V], Exp2[A, V], V, mutable.MutableList[Exp2[A, V]]]
    with ref.Eval[A[-?, ?, V]] {

  override def propagate(c: mutable.MutableList[Exp2[A, V]]) =
    new fullsub.Eval[A, V] with variant.Eval[A, V] {
      override def apply(e: Exp2[A, V]): Exp2[A, V] = Eval.this.apply(e)(c)

      override def subst(m: Map[String, Exp2[A, V]]): A[Exp2[A, V], Exp2[A, V], V] = Eval.this.subst(m)

      override val isVal: A[Exp2[A, V], Boolean, V] = Eval.this.isVal
    }
}

object Eval extends Eval[Term, Exp[Type]] with Impl[mutable.MutableList[Exp2[Term, Exp[Type]]] => Exp2[Term, Exp[Type]]] {
  override val isVal: Term[Exp2[Term, Exp[Type]], Boolean, Exp[Type]] = IsVal

  override def subst(m: Map[String, Exp2[Term, Exp[Type]]]) = new SubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V]
  with fullsub.IsVal[A, V] with variant.IsVal[A, V] with ref.IsVal[A[-?, ?, V]]

object IsVal extends IsVal[Term, Exp[Type]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Term[R, E, F], V] extends Transform[A, V]
  with fullsub.Subst[A, V] with variant.Subst[A, V]

class SubstImpl(mp: Map[String, Exp2[Term, Exp[Type]]]) extends Subst[Term, Exp[Type]] with Impl[Exp2[Term, Exp[Type]]] {
  override val m: Map[String, Exp2[Term, Exp[Type]]] = mp
}
