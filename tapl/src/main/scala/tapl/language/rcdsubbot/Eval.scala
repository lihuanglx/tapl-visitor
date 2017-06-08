package tapl.language.rcdsubbot

import tapl.common._
import tapl.component.typedrecord
import tapl.language.bot
import tapl.language.rcdsubbot.Alg._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with typedrecord.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] with bot.Eval[A, V]

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
  override val isVal: Alg[TExp[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal
  override val subst: (String, TExp[Alg, Exp[TAlg]]) => Alg[TExp[Alg, Exp[TAlg]], TExp[Alg, Exp[TAlg]], Exp[TAlg]] =
    (_x, _e) => new Subst[Alg, Exp[TAlg]] with Impl[TExp[Alg, Exp[TAlg]]] {
      override val x: String = _x
      override val e: TExp[Alg, Exp[TAlg]] = _e
    }
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V] with bot.IsVal[A, V]
  with typedrecord.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with bot.Subst[A, V]
