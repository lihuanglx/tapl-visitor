package tapl.language.fullisorec

import tapl.common._
import tapl.language.fullsimple
import tapl.language.fullisorec.Alg.{Query, Transform}
import tapl.language.fullisorec.Alg.Factory._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V] with fullsimple.Eval[A, V] {
  override def tmFold(e: TExp[A, V], t: V): TExp[A, V] =
    TmFold[A, V](if (e(isVal)) e else apply(e), t)

  override def tmUnfold(e: TExp[A, V], t: V): TExp[A, V] =
    if (e(isVal)) e match {
      case TmFold(v, _) => v
      case _ => typeError()
    } else {
      TmUnfold[A, V](apply(e), t)
    }
}

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val isVal: Alg[TExp[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override val subst: (String, TExp[Alg, Exp[TAlg]]) => Alg[TExp[Alg, Exp[TAlg]], TExp[Alg, Exp[TAlg]], Exp[TAlg]] =
    (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V] with fullsimple.IsVal[A, V] {
  override def tmFold(e: TExp[A, V], t: V): Boolean = apply(e)
}

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with fullsimple.Subst[A, V]

class SubstImpl(_x: String, _e: TExp[Alg, Exp[TAlg]]) extends Subst[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val x: String = _x
  override val e: TExp[Alg, Exp[TAlg]] = _e
}
