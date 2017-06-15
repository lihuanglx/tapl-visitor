package tapl.language.fullrecon

import tapl.common._
import tapl.component.let
import tapl.language.recon
import tapl.language.fullrecon.Alg.Factory._
import tapl.language.fullrecon.Alg.{Query, Transform}

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with recon.Eval[A, V] with let.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def tmUAbs(x: String, e: TExp[A, V]): TExp[A, V] = TmUAbs(x, e)

  override def tmApp(e1: TExp[A, V], e2: TExp[A, V]): TExp[A, V] =
    if (e1(isVal))
      if (e2(isVal)) e1 match {
        case TmAbs(x, _, body) => body(subst(x, e2))
        case TmUAbs(x, body) => body(subst(x, e2))
        case _ => typeError()
      } else TmApp[({type lam[-X, Y] = A[X, Y, V]})#lam](e1, apply(e2))
    else TmApp[({type lam[-X, Y] = A[X, Y, V]})#lam](apply(e1), e2)
}

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val isVal: Alg[TExp[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override def subst(m: Map[String, TExp[Alg, Exp[TAlg]]]) = new SubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V] with recon.IsVal[A, V] {
  override def tmUAbs(x: String, e: TExp[A, V]): Boolean = true
}

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with recon.Subst[A, V]
  with let.Subst[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def tmUAbs(x: String, e: TExp[A, V]): TExp[A, V] = TmUAbs(x, if (m.contains(x)) e else apply(e))
}

class SubstImpl(mp: Map[String, TExp[Alg, Exp[TAlg]]]) extends Subst[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val m: Map[String, TExp[Alg, Exp[TAlg]]] = mp
}
