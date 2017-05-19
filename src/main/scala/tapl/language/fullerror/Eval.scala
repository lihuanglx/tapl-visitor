package tapl.language.fullerror

import tapl.common.Exp
import tapl.common.Util._
import tapl.component.{typed2, typedbool}

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with typed2.Eval[A, V] with typedbool.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmError(): E3[A, V] = CError[A, V]()

  override def TmTry(e1: E3[A, V], e2: E3[A, V]): E3[A, V] = e1 match {
    case CError() => e2
    case _ => if (e1(isVal)) e1 else CTry[A, V](apply(e1), e2)
  }

  override def TmApp(e1: E3[A, V], e2: E3[A, V]): E3[A, V] = (e1, e2) match {
    case (CError(), _) => e1
    case (_, CError()) => e2
    case _ => super.TmApp(e1, e2)
  }
}

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[E3[Alg, Exp[TAlg]]] {
  override val isVal: Alg[E3[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override val subst: (String, E3[Alg, Exp[TAlg]]) => Alg[E3[Alg, Exp[TAlg]], E3[Alg, Exp[TAlg]], Exp[TAlg]] =
    (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, E, -F], V] extends Query[E3[A, V], Boolean, V]
  with typed2.IsVal[A, V] with typedbool.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with typed2.Subst[A, V]

class SubstImpl(_x: String, _e: E3[Alg, Exp[TAlg]]) extends Subst[Alg, Exp[TAlg]] with Impl[E3[Alg, Exp[TAlg]]] {
  override val x: String = _x
  override val e: E3[Alg, Exp[TAlg]] = _e
}