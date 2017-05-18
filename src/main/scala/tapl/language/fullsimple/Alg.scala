package tapl.language.fullsimple

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.{simple, variant}

trait Alg[-R, E, -F] extends simple.Alg[R, E, F] with variant.Alg[R, E, F]

trait TAlg[-F, T] extends simple.TAlg[F, T] with variant.TAlg[F, T]

trait Factory extends simple.Factory with variant.Factory

object Factory extends Factory

trait TFactory extends simple.TFactory with variant.TFactory

object TFactory extends TFactory

trait Impl[T] extends Alg[E3[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: E3[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
