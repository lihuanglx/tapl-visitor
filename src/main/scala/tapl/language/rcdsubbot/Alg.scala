package tapl.language.rcdsubbot

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.typedrecord
import tapl.language.bot

trait Alg[-R, E, -F] extends typedrecord.Alg[R, E] with bot.Alg[R, E, F]

trait TAlg[-F, T] extends typedrecord.TAlg[F, T] with bot.TAlg[F, T]

trait Factory extends typedrecord.Factory with bot.Factory

object Factory extends Factory

trait TFactory extends typedrecord.TFactory with bot.TFactory

object TFactory extends TFactory

trait Impl[T] extends Alg[E3[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: E3[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}

