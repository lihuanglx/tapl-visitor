package tapl.language.fullerror

import macros.Language
import tapl.common._
import tapl.component.typedbool
import tapl.language.bot

@Language
trait Alg[-R, E, -F] extends bot.Alg[R, E, F] with typedbool.Alg[R, E] {
  def tmError(): E

  def tmTry(e1: R, e2: R): E
}

@Language
trait TAlg[-F, T] extends bot.TAlg[F, T] with typedbool.TAlg[F, T]

trait Impl[T] extends Alg[Exp2[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: Exp2[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
