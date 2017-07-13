package tapl.language.rcdsubbot

import tapl.common._
import tapl.component.typedrecord
import tapl.language.bot
import tapl.language.rcdsubbot.Alg._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[Exp2[A, V], Exp2[A, V], V]
  with typedrecord.Eval[A[-?, ?, V]] with bot.Eval[A, V]

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[Exp2[Alg, Exp[TAlg]]] {
  override val isVal: Alg[Exp2[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override def subst(m: Map[String, Exp2[Alg, Exp[TAlg]]]) = new SubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V]
  with bot.IsVal[A, V] with typedrecord.IsVal[A[-?, ?, V]]

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with bot.Subst[A, V]

class SubstImpl(mp: Map[String, Exp2[Alg, Exp[TAlg]]]) extends Subst[Alg, Exp[TAlg]] with Impl[Exp2[Alg, Exp[TAlg]]] {
  override val m: Map[String, Exp2[Alg, Exp[TAlg]]] = mp
}
