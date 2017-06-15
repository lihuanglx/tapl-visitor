package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot
import tapl.language.fullerror.Alg.{Query, Transform}
import tapl.language.fullerror.Alg.Factory._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with bot.Eval[A, V] with typedbool.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def tmError(): TExp[A, V] = TmError[A, V]()

  override def tmTry(e1: TExp[A, V], e2: TExp[A, V]): TExp[A, V] = e1 match {
    case TmError() => e2
    case _ => if (e1(isVal)) e1 else TmTry[A, V](apply(e1), e2)
  }

  override def tmApp(e1: TExp[A, V], e2: TExp[A, V]): TExp[A, V] = (e1, e2) match {
    case (TmError(), _) => e1
    case (_, TmError()) => e2
    case _ => super.tmApp(e1, e2)
  }
}

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val isVal: Alg[TExp[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override def subst(m: Map[String, TExp[Alg, Exp[TAlg]]]) = new SubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V]
  with bot.IsVal[A, V] with typedbool.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def tmError(): Boolean = true
}

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with bot.Subst[A, V]

class SubstImpl(mp: Map[String, TExp[Alg, Exp[TAlg]]]) extends Subst[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val m: Map[String, TExp[Alg, Exp[TAlg]]] = mp
}
