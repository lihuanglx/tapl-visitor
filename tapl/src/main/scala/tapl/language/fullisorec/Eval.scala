package tapl.language.fullisorec

import tapl.common._
import tapl.language.fullsimple

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V] with fullsimple.Eval[A, V] {
  override def TmFold(e: E3[A, V], t: V): E3[A, V] =
    CFold[A, V](if (e(isVal)) e else apply(e), t)

  override def TmUnfold(e: E3[A, V], t: V): E3[A, V] =
    if (e(isVal)) e match {
      case CFold(v, _) => v
      case _ => typeError()
    } else {
      CUnFold[A, V](apply(e), t)
    }
}

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[E3[Alg, Exp[TAlg]]] {
  override val isVal: Alg[E3[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override val subst: (String, E3[Alg, Exp[TAlg]]) => Alg[E3[Alg, Exp[TAlg]], E3[Alg, Exp[TAlg]], Exp[TAlg]] =
    (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, E, -F], V] extends Query[E3[A, V], Boolean, V] with fullsimple.IsVal[A, V] {
  override def TmFold(e: E3[A, V], t: V): Boolean = apply(e)
}

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with fullsimple.Subst[A, V]

class SubstImpl(_x: String, _e: E3[Alg, Exp[TAlg]]) extends Subst[Alg, Exp[TAlg]] with Impl[E3[Alg, Exp[TAlg]]] {
  override val x: String = _x
  override val e: E3[Alg, Exp[TAlg]] = _e
}
