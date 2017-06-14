package tapl.language.fullpoly

import tapl.common._
import tapl.component._
import tapl.language.fullpoly.Alg.{Query, Transform, MapSnd}
import tapl.language.fullpoly.Alg.Factory._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[TExp[A, Exp[B]], TExp[A, Exp[B]], Exp[B]]
    with typed.Eval[A, Exp[B]] with extension.Eval[A, Exp[B]] with pack.Eval[A, Exp[B]] {

  val mapSnd: (Exp[B] => Exp[B]) => A[TExp[A, Exp[B]], TExp[A, Exp[B]], Exp[B]]

  val tSubst: (String, Exp[B]) => B[Exp[B], Exp[B]]

  override def tmTAbs(x: String, e: TExp[A, Exp[B]]): TExp[A, Exp[B]] = TmTAbs(x, e)

  override def tmTApp(e: TExp[A, Exp[B]], t: Exp[B]): TExp[A, Exp[B]] =
    if (e(isVal)) e match {
      case TmTAbs(x, b) => b(mapSnd(_ (tSubst(x, t))))
      case _ => typeError()
    } else TmTApp(apply(e), t)
}

object Eval extends Eval[Alg, TAlg] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val isVal: Alg[TExp[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override val subst: (String, TExp[Alg, Exp[TAlg]]) => Alg[TExp[Alg, Exp[TAlg]], TExp[Alg, Exp[TAlg]], Exp[TAlg]] =
    (x, e) => new SubstImpl(x, e)

  override val mapSnd: ((Exp[TAlg]) => Exp[TAlg]) => Alg[TExp[Alg, Exp[TAlg]], TExp[Alg, Exp[TAlg]], Exp[TAlg]] =
    f => new MapSnd[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
      override def mp(t: Exp[TAlg]): Exp[TAlg] = f(t)
    }

  override val tSubst: (String, Exp[TAlg]) => TAlg[Exp[TAlg], Exp[TAlg]] =
    (x, e) => new TSubstImpl(x, e)
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V]
  with typed.IsVal[A, V] with extension.IsVal[A, V] {

  override def tmTAbs(x: String, e: TExp[A, V]): Boolean = true
}

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with
  typed.Subst[A, V] with extension.Subst[A, V] with pack.Subst[A, V]

class SubstImpl(_x: String, _e: TExp[Alg, Exp[TAlg]]) extends Subst[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val x: String = _x
  override val e: TExp[Alg, Exp[TAlg]] = _e
}
