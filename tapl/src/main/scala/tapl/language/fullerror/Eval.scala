package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with bot.Eval[A, V] with typedbool.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmError(): TExp[A, V] = CError[A, V]()

  override def TmTry(e1: TExp[A, V], e2: TExp[A, V]): TExp[A, V] = e1 match {
    case CError() => e2
    case _ => if (e1(isVal)) e1 else CTry[A, V](apply(e1), e2)
  }

  override def tmApp(e1: TExp[A, V], e2: TExp[A, V]): TExp[A, V] = (e1, e2) match {
    case (CError(), _) => e1
    case (_, CError()) => e2
    case _ => super.tmApp(e1, e2)
  }
}

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val isVal: Alg[TExp[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override val subst: (String, TExp[Alg, Exp[TAlg]]) => Alg[TExp[Alg, Exp[TAlg]], TExp[Alg, Exp[TAlg]], Exp[TAlg]] =
    (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V]
  with bot.IsVal[A, V] with typedbool.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmError(): Boolean = true
}

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with bot.Subst[A, V]

class SubstImpl(_x: String, _e: TExp[Alg, Exp[TAlg]]) extends Subst[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val x: String = _x
  override val e: TExp[Alg, Exp[TAlg]] = _e
}
