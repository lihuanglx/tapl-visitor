package tapl.language.fullrecon

import macros.Visitor
import tapl.common._
import tapl.component.let
import tapl.language.recon

@Visitor
trait Alg[-R, E, -F] extends recon.Alg[R, E, F] with let.Alg[R, E] {
  def tmUAbs(x: String, e: R): E
}

@Visitor
trait TAlg[-F, T] extends recon.TAlg[F, T]

trait Impl[T] extends Alg[TExp[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: TExp[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}

