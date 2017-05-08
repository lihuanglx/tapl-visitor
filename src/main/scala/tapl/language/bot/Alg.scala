package tapl.language.bot

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.{topbot, typed}

trait Alg[-R, E, -F] extends typed.Alg[R, E, F]

trait TAlg[-F, T] extends typed.TAlg[F, T] with topbot.TAlg[F, T]

trait Factory extends typed.Factory

trait TFactory extends typed.TFactory with topbot.TFactory

object Factory extends Factory

object TFactory extends TFactory

trait Impl[T] extends Alg[E3[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: E3[Alg, Exp[TAlg]]): T = e(this)
}