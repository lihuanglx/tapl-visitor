package tapl.language.simplebool

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.{typed, typedbool}

trait Alg[-R, E, -F] extends typed.Alg[R, E, F] with typedbool.Alg[R, E]

trait TAlg[-F, T] extends typed.TAlg[F, T] with typedbool.TAlg[F, T]

trait Factory extends typed.Factory with typedbool.Factory

object Factory extends Factory

trait TFactory extends typed.TFactory with typedbool.TFactory

trait Impl[T] extends Alg[E3[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: E3[Alg, Exp[TAlg]]): T = e(this)
}
