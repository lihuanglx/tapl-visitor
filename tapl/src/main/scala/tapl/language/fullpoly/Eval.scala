package tapl.language.fullpoly

import tapl.common._
import tapl.component._
import tapl.language.fullpoly.Alg.{Query, Transform, Map2}
import tapl.language.fullpoly.Alg.Factory._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[Exp2[A, Exp[B]], Exp2[A, Exp[B]], Exp[B]]
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

object Eval extends Eval[Alg, TAlg] with Impl[Exp2[Alg, Exp[TAlg]]] {
  override val isVal: Alg[Exp2[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override def subst(m: Map[String, Exp2[Alg, Exp[TAlg]]]) = new SubstImpl(m)

  override val map2: Alg[Exp2[Alg, Exp[TAlg]], ((Exp[TAlg]) => Exp[TAlg]) => Exp2[Alg, Exp[TAlg]], Exp[TAlg]] =
    new Map2[Alg, Exp[TAlg]] with Impl[((Exp[TAlg]) => Exp[TAlg]) => Exp2[Alg, Exp[TAlg]]]

  override def tSubst(m: Map[String, Exp[TAlg]]): TAlg[Exp[TAlg], Exp[TAlg]] = new TSubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V]
  with typed.IsVal[A, V] with extension.IsVal[A, V] with pack.IsVal[A, V] {

  override def tmTAbs(x: String, e: Exp2[A, V]): Boolean = true
}

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with
  typed.Subst[A, V] with extension.Subst[A, V] with pack.Subst[A, V]

class SubstImpl(mp: Map[String, Exp2[Alg, Exp[TAlg]]]) extends Subst[Alg, Exp[TAlg]] with Impl[Exp2[Alg, Exp[TAlg]]] {
  override val m: Map[String, Exp2[Alg, Exp[TAlg]]] = mp
}
