package tapl.language.recon

import tapl.common._
import tapl.component.typed
import tapl.language.tyarith

trait Alg[-R, E, -F] extends tyarith.Alg[R, E] with typed.Alg[R, E, F]

trait TAlg[-F, T] extends tyarith.TAlg[F, T] with typed.TAlg[F, T]

trait Factory extends tyarith.Alg.Factory with typed.Factory

object Factory extends Factory

trait TFactory extends tyarith.TAlg.Factory with typed.TFactory

trait Impl[T] extends Alg[E3[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: E3[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
