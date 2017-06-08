package tapl.language.recon

import tapl.common._
import tapl.component.typed
import tapl.language.tyarith

trait Alg[-R, E, -F] extends tyarith.Alg[R, E] with typed.Alg[R, E, F]

trait TAlg[-F, T] extends tyarith.TAlg[F, T] with typed.TAlg[F, T]

trait Factory extends tyarith.Alg.Factory with typed.Alg.Factory

object Factory extends Factory

trait TFactory extends tyarith.TAlg.Factory with typed.TAlg.Factory

trait Impl[T] extends Alg[TExp[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: TExp[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
