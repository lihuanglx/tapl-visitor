package tapl.language.fullpoly

import tapl.common._
import tapl.component._
import tapl.language.fullpoly.Term.{Query, Transform, Map2}
import tapl.language.fullpoly.Term.Factory._

trait Eval[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], Exp2[A, Exp[B]], Exp[B]]
    with typed.Eval[A, Exp[B]] with extension.Eval[A, Exp[B]] with pack.Eval[A, Exp[B]] {

  val map2: A[Exp2[A, Exp[B]], (Exp[B] => Exp[B]) => Exp2[A, Exp[B]], Exp[B]]

  def tSubst(m: Map[String, Exp[B]]): B[Exp[B], Exp[B]]

  override def tmTAbs(x: String, e: Exp2[A, Exp[B]]): Exp2[A, Exp[B]] = TmTAbs(x, e)

  override def tmTApp(e: Exp2[A, Exp[B]], t: Exp[B]): Exp2[A, Exp[B]] =
    if (e(isVal)) e match {
      case TmTAbs(x, b) => b(map2)(_ (tSubst(Map(x -> t))))
      case _ => typeError()
    } else TmTApp(apply(e), t)
}

object Eval extends Eval[Term, Type] with Impl[Exp2[Term, Exp[Type]]] {
  override val isVal: Term[Exp2[Term, Exp[Type]], Boolean, Exp[Type]] = IsVal

  override def subst(m: Map[String, Exp2[Term, Exp[Type]]]) = new SubstImpl(m)

  override val map2: Term[Exp2[Term, Exp[Type]], ((Exp[Type]) => Exp[Type]) => Exp2[Term, Exp[Type]], Exp[Type]] =
    new Map2[Term, Exp[Type]] with Impl[((Exp[Type]) => Exp[Type]) => Exp2[Term, Exp[Type]]]

  override def tSubst(m: Map[String, Exp[Type]]): Type[Exp[Type], Exp[Type]] = new TSubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V]
  with typed.IsVal[A, V] with extension.IsVal[A, V] with pack.IsVal[A, V] {

  override def tmTAbs(x: String, e: Exp2[A, V]): Boolean = true
}

object IsVal extends IsVal[Term, Exp[Type]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Term[R, E, F], V] extends Transform[A, V] with
  typed.Subst[A, V] with extension.Subst[A, V] with pack.Subst[A, V]

class SubstImpl(mp: Map[String, Exp2[Term, Exp[Type]]]) extends Subst[Term, Exp[Type]] with Impl[Exp2[Term, Exp[Type]]] {
  override val m: Map[String, Exp2[Term, Exp[Type]]] = mp
}
