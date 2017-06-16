package tapl.language.recon

import macros.Visitor
import tapl.common._
import tapl.component.typed
import tapl.language.tyarith

@Visitor
trait Alg[-R, E, -F] extends tyarith.Alg[R, E] with typed.Alg[R, E, F]

@Visitor
trait TAlg[-F, T] extends tyarith.TAlg[F, T] with typed.TAlg[F, T]

trait Impl[T] extends Alg[Exp2[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: Exp2[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
