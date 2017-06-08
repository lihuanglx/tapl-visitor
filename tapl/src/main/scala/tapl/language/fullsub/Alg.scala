package tapl.language.fullsub

import tapl.common._
import tapl.component.{simple, top}

trait Alg[-R, E, -F] extends simple.Alg[R, E, F]

trait TAlg[-F, T] extends simple.TAlg[F, T] with top.TAlg[F, T]

trait Factory extends simple.Alg.Factory

object Factory extends Factory

trait TFactory extends simple.TAlg.Factory with top.TAlg.Factory

object TFactory extends TFactory

trait Impl[T] extends Alg[TExp[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: TExp[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
