package tapl.language.fullequirec

import tapl.common._
import tapl.component.{extension, variant}
import tapl.language.equirec
import tapl.language.fullequirec.Alg._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with equirec.Eval[A, V] with extension.Eval[A, V] with variant.Eval[A, V]

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val isVal: Alg[TExp[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override val subst: (String, TExp[Alg, Exp[TAlg]]) => Alg[TExp[Alg, Exp[TAlg]], TExp[Alg, Exp[TAlg]], Exp[TAlg]] =
    (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V]
  with equirec.IsVal[A, V] with extension.IsVal[A, V] with variant.IsVal[A, V]

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V]
  with equirec.Subst[A, V] with extension.Subst[A, V] with variant.Subst[A, V]

class SubstImpl(_x: String, _e: TExp[Alg, Exp[TAlg]]) extends Subst[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val x: String = _x
  override val e: TExp[Alg, Exp[TAlg]] = _e
}
