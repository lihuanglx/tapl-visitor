package tapl.language.rcdsubbot

import macros.Language
import tapl.common._
import tapl.component.typedrecord
import tapl.language.bot

@Language
trait Alg[-R, E, -F] extends typedrecord.Alg[R, E] with bot.Alg[R, E, F]

@Language
trait TAlg[-F, T] extends typedrecord.TAlg[F, T] with bot.TAlg[F, T]

trait Impl[T] extends Alg[Exp2[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: Exp2[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
