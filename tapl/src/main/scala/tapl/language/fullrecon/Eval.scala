package tapl.language.fullrecon

import tapl.common._
import tapl.component.let
import tapl.language.recon
import tapl.language.fullrecon.Alg._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with recon.Eval[A, V] with let.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val isVal: Alg[TExp[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override def subst(m: Map[String, TExp[Alg, Exp[TAlg]]]) = new SubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V] with recon.IsVal[A, V]

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with recon.Subst[A, V]
  with let.Subst[({type lam[-X, Y] = A[X, Y, V]})#lam]

class SubstImpl(mp: Map[String, TExp[Alg, Exp[TAlg]]]) extends Subst[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val m: Map[String, TExp[Alg, Exp[TAlg]]] = mp
}
