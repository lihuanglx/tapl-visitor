package tapl.language.fullsub

import macros.Visitor
import tapl.common._
import tapl.component.{typed, extension, top}

@Visitor
trait Alg[-R, E, -F] extends typed.Alg[R, E, F] with extension.Alg[R, E, F]

@Visitor
trait TAlg[-F, T] extends typed.TAlg[F, T] with extension.TAlg[F, T] with top.TAlg[F, T]

trait Impl[T] extends Alg[TExp[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: TExp[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
