package tapl.language.fullsimple

import macros.Visitor
import tapl.common._
import tapl.component.{simple, variant}

@Visitor
trait Alg[-R, E, -F] extends simple.Alg[R, E, F] with variant.Alg[R, E, F]

@Visitor
trait TAlg[-F, T] extends simple.TAlg[F, T] with variant.TAlg[F, T]

trait Impl[T] extends Alg[TExp[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: TExp[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
