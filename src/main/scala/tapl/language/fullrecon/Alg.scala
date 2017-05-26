package tapl.language.fullrecon

import tapl.common._
import tapl.component.let
import tapl.language.recon

trait Alg[-R, E, -F] extends recon.Alg[R, E, F] with let.Alg[R, E]

trait TAlg[-F, T] extends recon.TAlg[F, T]

trait Factory extends recon.Factory with let.Factory

object Factory extends Factory

trait TFactory extends recon.TFactory

trait Impl[T] extends Alg[E3[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: E3[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}

