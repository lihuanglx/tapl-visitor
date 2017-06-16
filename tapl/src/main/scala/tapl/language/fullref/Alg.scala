package tapl.language.fullref

import macros.Visitor
import tapl.common._
import tapl.component.{variant, ref}
import tapl.language.fullsub

@Visitor
trait Alg[-R, E, -F] extends fullsub.Alg[R, E, F] with variant.Alg[R, E, F] with ref.Alg[R, E]

@Visitor
trait TAlg[-F, T] extends fullsub.TAlg[F, T] with variant.TAlg[F, T] with ref.TAlg[F, T] {
  def tySource(t: F): T

  def tySink(t: F): T
}

trait Impl[T] extends Alg[Exp2[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: Exp2[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
