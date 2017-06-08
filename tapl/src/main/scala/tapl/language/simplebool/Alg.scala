package tapl.language.simplebool

import tapl.common._
import tapl.component.{typed, typedbool}

trait Alg[-R, E, -F] extends typed.Alg[R, E, F] with typedbool.Alg[R, E]

trait TAlg[-F, T] extends typed.TAlg[F, T] with typedbool.TAlg[F, T]

trait Factory extends typed.Factory with typedbool.Alg.Factory

object Factory extends Factory

trait TFactory extends typed.TFactory with typedbool.TAlg.Factory

trait Impl[T] extends Alg[E3[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: E3[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
