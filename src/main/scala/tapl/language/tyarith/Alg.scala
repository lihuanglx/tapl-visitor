package tapl.language.tyarith

import tapl.common.Exp
import tapl.component.{typedbool, typednat}

trait Alg[-R, E] extends typedbool.Alg[R, E] with typednat.Alg[R, E]

trait TAlg[-F, T] extends typedbool.TAlg[F, T] with typednat.TAlg[F, T]

trait Factory extends typedbool.Factory with typednat.Factory

object Factory extends Factory

trait TFactory extends typedbool.TFactory with typednat.TFactory

object TFactory extends TFactory

trait Impl[T] extends Alg[Exp[Alg], T] {
  override def apply(e: Exp[Alg]): T = e(this)
}
